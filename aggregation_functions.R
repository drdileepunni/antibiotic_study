library(tableone)
library(ggplot2)
library(table1)
library(rvest)
library(dplyr)

source("./cleaning_data.R")
source("./helper_functions.R")

clean_abx_dataset <- function(all_antibiotics_path) {

    # Loading data
    abx_df <- read.csv(all_antibiotics_path)

    cat("\n==========================================================\n")

    n_rows_initial <- count(abx_df)[1,1]
    print("Loaded all_antibiotics dataframe..")
    print(glue("{n_rows_initial} rows in the dataframe.."))

    cat("\n==========================================================\n")
    
    abx_df['covid_bin'] <- ifelse(((abx_df$covid=='suspected') | (abx_df$covid=='positive')), 1, 0)
    print("Added covid binary column..")
    print(table(abx_df$covid_bin))
    
    cat("\n==========================================================\n")

    # Replacing unstandard names with standard ones
    for (v in ls(replace_dict)) {
        abx_df <- replace_name(abx_df, v, replace_dict[[v]])
        string <- glue("{v} replaced by {replace_dict[[v]]}")
        print(string)
    }

    cat("\n==========================================================\n")

    print("Filtering bad vented hour rows..")

    #Filtering out entries with negative vent hours
    abx_df <- filter(abx_df, (abx_df$vented_hours >=0) & (abx_df$vented_hours<=300))

    n_rows_after <- count(abx_df)[1,1]
    print(glue("{n_rows_initial - n_rows_after} rows filtered.."))

    #Defining some new columns
    abx_df <- add_columns(abx_df)
    
    cleaned_abx_path <- "./data/abx_df_cleaned.csv"
    write.csv(abx_df, cleaned_abx_path)

    cat("\n==========================================================\n")

    print(glue("{n_rows_after} rows in final.."))
    print("Stored cleaned data to ./data/abx_df_cleaned.csv !!")
    
    return(cleaned_abx_path)

}

get_summary_table <- function(cleaned_abx_path) {

    # Loading cleaned all antibiotic dataframe
    abx_df <- read.csv(cleaned_abx_path)

    # Creating admissions with antibiotic dataframe from that 
    distinct_admissions <- abx_df %>% distinct(admission_id, .keep_all = TRUE)

    # Further cleaning steps

    ## Changing 0 to NA so as to drop them out of mean/median calculation
    distinct_admissions$hfnc_hours[distinct_admissions$hfnc_hours == 0] <- NA
    distinct_admissions$niv_hours[distinct_admissions$niv_hours == 0] <- NA
    distinct_admissions$vented_hours[distinct_admissions$vented_hours == 0] <-NA

    distinct_admissions$vent <- 
      factor(distinct_admissions$vent, levels=c(1,0),
             labels=c("Vented", 
                      "Not vented"))
    distinct_admissions$abx_watch <- 
      factor(distinct_admissions$abx_watch, levels=c(1,0),
             labels=c("Yes", 
                      "No"))
    distinct_admissions$abx_reserve <- 
      factor(distinct_admissions$abx_reserve, levels=c(1,0),
             labels=c("Yes", 
                      "No"))
    distinct_admissions$covid_specific_abx <- 
      factor(distinct_admissions$covid_specific_abx, levels=c(1,0),
             labels=c("Yes", 
                      "No"))
    distinct_admissions$hfnc <-factor(distinct_admissions$hfnc, levels = c(1,0),
                         labels = c("Yes", "No"))
    distinct_admissions$niv <-factor(distinct_admissions$niv, levels = c(1,0),
                         labels = c("Yes", "No"))
    distinct_admissions$imv <- factor(distinct_admissions$imv, levels = c(1,0), labels = c("Yes", "No"))

    distinct_admissions$covid <-factor(distinct_admissions$covid, levels=c("", "positive", "suspected"),labels=c("COVID Negative", "COVID Positve", "COVID Suspected"))


    label(distinct_admissions$gender)       <- "Gender"
    label(distinct_admissions$age)       <- "Age"
    label(distinct_admissions$discharge_disposition)     <- "Outcome"
    label(distinct_admissions$apache_score) <- "APACHE score"
    label(distinct_admissions$vent)       <- "Ventilation status"
    label(distinct_admissions$abx_watch)       <- "Recieved watch group antibiotics"
    label(distinct_admissions$abx_reserve)     <- "Recieved reserve group antibiotics"
    label(distinct_admissions$niv) <- "NIV use"
    label(distinct_admissions$hfnc) <- "HFNC use"
    label(distinct_admissions$imv_hours) <- "IMV hours"
    label(distinct_admissions$niv_hours) <- "NIV hours"
    label(distinct_admissions$hfnc_hours) <- "HFNC hours"
    label(distinct_admissions$vented_hours) <- "Vented hours"

    units(distinct_admissions$age)       <- "years"

    # Creation of summary table
    summary_table <- table1::table1(~ gender + age + discharge_disposition + apache_score +  abx_watch + 
                                    abx_reserve +covid_specific_abx+  vent + hfnc + niv + imv + hfnc_hours 
                                    + niv_hours + vented_hours| covid, data=distinct_admissions )


    summary_df <- as.data.frame(read_html(summary_table) %>% html_table(fill=TRUE))
    
    return(summary_df)

}

get_admission_related_details <- function(all_admissions_path) {

    adm_df <- read.csv(all_admissions_path)
    adm_df$received_abx[is.na(adm_df$received_abx)] <- 0

    # Removing bad vent hours
    adm_df <- filter(adm_df, (adm_df$vented_hours >=0) & (adm_df$vented_hours<=300))

    # Removing admissions with no med orders
    adm_df <- filter(adm_df, adm_df$no_medorders=='False')

    # Getting and printing everything in the results section
    total_admissions <- count(adm_df)[1,1]
    covid_division <- table(adm_df$covid)
    total_hospitals <- length(unique(adm_df$name.2))
    received_at_least_one_abx <- sum(adm_df$received_abx, na.rm=TRUE)

    covid_division_wavewise <- table(adm_df$covid, adm_df$wave)

    print(glue("Total admissions: {total_admissions}"))
    print(glue("Total admissions that received at least one antibiotic: {received_at_least_one_abx}"))

    cat("\n==========================================================\n")

    print("Covid positive, negative, suspect division (count):")
    print(covid_division)

    cat("\n==========================================================\n")

    print("Covid positive, negative, suspect division (percent):")
    print(prop.table(covid_division))

    cat("\n==========================================================\n")

    print("Covid positive, negative, suspect division wavewise:")
    print(covid_division_wavewise)

    cat("\n==========================================================\n")

    first_wave_total_adm <- covid_division_wavewise[4] + covid_division_wavewise[5] + covid_division_wavewise[6]
    second_wave_total_adm <- covid_division_wavewise[7] + covid_division_wavewise[8] + covid_division_wavewise[9]
    not_first_or_second_total_adm <- covid_division_wavewise[1] + covid_division_wavewise[2] + covid_division_wavewise[3]

    print(glue("Total admissions in first wave: {first_wave_total_adm}"))
    print(glue("Total admissions in second wave: {second_wave_total_adm}"))
    print(glue("Total admissions in neither waves: {not_first_or_second_total_adm}"))

    cat("\n==========================================================\n")

    first_wave_adm_df <- filter(adm_df, adm_df$wave==1)
    second_wave_adm_df <- filter(adm_df, adm_df$wave==2)

    print("First wave proportion of Covid cases:")
    prop.table(table(first_wave_adm_df$covid))

    first_wave_total_covid <- table(first_wave_adm_df$covid)[2] + table(first_wave_adm_df$covid)[3]
    first_wave_prop_suspect <- table(first_wave_adm_df$covid)[3]/first_wave_total_covid
    print(glue("First wave total covid positive:{first_wave_total_covid}"))
    print(glue("First wave proportion of covid suspect:{first_wave_prop_suspect}"))

    cat("\n==========================================================\n")

    print("Second wave proportion of Covid cases:")
    prop.table(table(second_wave_adm_df$covid))

    second_wave_total_covid <- table(second_wave_adm_df$covid)[2] + table(second_wave_adm_df$covid)[3]
    second_wave_prop_suspect <- table(second_wave_adm_df$covid)[3]/second_wave_total_covid
    print(glue("Second wave total covid positive:{second_wave_total_covid}"))
    print(glue("Second wave proportion of covid suspect:{second_wave_prop_suspect}"))

    cat("\n==========================================================\n")

    tab <- table(first_wave_adm_df$covid, first_wave_adm_df$received_abx)
    proportion_received_abx_first_wave <- round(100-((tab[2] + tab[3])/(tab[5] + tab[6])*100), 1)
    print(glue("Proportion of admissions that received antibiotics during first wave: {proportion_received_abx_first_wave}"))

    tab <- table(second_wave_adm_df$covid, second_wave_adm_df$received_abx)
    proportion_received_abx_second_wave <- round(100-((tab[2] + tab[3])/(tab[5] + tab[6])*100), 1)
    print(glue("Proportion of admissions that received antibiotics during second wave: {proportion_received_abx_second_wave}"))

}

get_abx_related_details <- function(cleaned_abx_path) {

    abx_df <- read.csv(cleaned_abx_path)

    print(glue("Total antibiotic prescriptions: {count(abx_df)[1,1]}"))

    cat("\n==========================================================\n")
    print("Received watch group antibiotics")
    table(abx_df$abx_watch)
    print(glue("Percent of patients who received watch group antibiotics: {round(prop.table(table(abx_df$abx_watch))[2]*100, 1)}"))

    cat("\n==========================================================\n")
    print("Received access group antibiotics")
    table(abx_df$abx_access)
    print(glue("Percent of patients who received access group antibiotics: {round(prop.table(table(abx_df$abx_access))[2]*100, 1)}"))

    cat("\n==========================================================\n")
    print("Received reserve group antibiotics")
    table(abx_df$abx_reserve)
    print(glue("Percent of patients who received reserve group antibiotics: {round(prop.table(table(abx_df$abx_reserve))[2]*100, 1)}"))


    cat("\n==========================================================\n")

    antibiotic_freq <- round(prop.table(table(abx_df$name))*100, 2) %>% as.data.frame %>% arrange(desc(Freq))
    antibiotic_top_freq <- antibiotic_freq[1:6,]

    print('Top prescribed antibiotics all patients')
    cat('\n')
    print(antibiotic_top_freq)

    cat("\n==========================================================\n")

    abx_covid <- filter(abx_df, ((abx_df$covid=='suspected') | (abx_df$covid=='positive')))
    covid_antibiotic_freq <- round(prop.table(table(abx_covid$name))*100, 2) %>% as.data.frame %>% arrange(desc(Freq))
    covid_antibiotic_top_freq <- covid_antibiotic_freq[1:6,]

    print('Top prescribed antibiotics covid patients')
    cat('\n')
    print(antibiotic_top_freq)

    cat("\n==========================================================\n")

    covid_abx_vent <- table1::table1(~ name  | vent, data=abx_covid)
    covid_abx_vent_df <- as.data.frame(read_html(covid_abx_vent) %>% html_table(fill=TRUE))

    write.csv(covid_abx_vent_df, "./data/covid_abx_use_vent_vs_nonvent.csv")
    print('Table too large to print, saved to data folder as covid_abx_use_vent_vs_nonvent.csv')

    antibiotics = c('piptaz', 'meropenem')

    for (antibiotic in antibiotics) {

        cat("\n==========================================================\n")

        print(glue("{antibiotic} usage among vented and non vented patients"))
        print(table(abx_covid$vent, abx_covid[[antibiotic]]))
        print(chisq.test(abx_covid$vent, abx_covid[[antibiotic]]))

    }

    abx_covid_only_waves <- filter(abx_covid, ((abx_covid$wave==1) | abx_covid$wave==2))

    cols <- c('abx_watch', 'abx_reserve')

    for (col in cols) {

        cat("\n==========================================================\n")

        tab <- table(abx_covid_only_waves$wave, abx_covid_only_waves[[col]])

        watch_wave1 <- round((tab[3]/(tab[3]+tab[1]))*100, 2)
        watch_wave2 <- round((tab[4]/(tab[4]+tab[2]))*100, 2)
        print(glue("Proportion of patient on {col} group in wave 1: {watch_wave1}"))
        print(glue("Proportion of patient on {col} group in wave 2: {watch_wave2}"))

        print(chisq.test(abx_covid_only_waves$wave, abx_covid_only_waves[[col]]))

    }

    cat("\n==========================================================\n")
    print("USAGE OF COVID ANTIBIOTICS AMONG COVID PATIENTS!!")

    covid_antibiotics <- c('Remdesivir', 'Oseltamivir', 'Azithromycin', 'Ivermectin', 'Favipiravir', 
                           'Hydroxychloroquine', 'Cefotaxime')

    for (name in covid_antibiotics) {

        print(name)
        tab <- table(abx_covid_only_waves$wave, abx_covid_only_waves[[name]])
        first_wave_percent <- round((tab[3]/(tab[3]+tab[1]))*100, 2)
        second_wave_percent <- round((tab[4]/(tab[4]+tab[2]))*100, 2)
        print(glue("{name} use in first wave: {first_wave_percent}"))
        print(glue("{name} use in second wave: {second_wave_percent}"))
        print(glue("Change is: {second_wave_percent - first_wave_percent}"))
        print(chisq.test(abx_covid_only_waves$wave, abx_covid_only_waves[[name]]))
        cat("\n==========================================================\n")

    }
    
    cat("\n==========================================================\n")
    print("USAGE OF ANTIBIOTICS GROUPS AMONG ALL PATIENTS!!")
    
    items <- c('abx_watch', 'abx_reserve', 'covid_specific_abx')
    
    for (item in items) {
      
      print("\n==========================================================\n")
      print(item)
      
      tab <- table(abx_covid_only_waves$wave, abx_covid_only_waves[[item]])
      
      first_wave_percent <- round((tab[3]/(tab[3]+tab[1]))*100, 2)
      second_wave_percent <- round((tab[4]/(tab[4]+tab[2]))*100, 2)
      
      print(glue("{item} use in first wave: {first_wave_percent}"))
      print(glue("{item} use in second wave: {second_wave_percent}"))
      print(glue("Change is: {second_wave_percent - first_wave_percent}"))
      
      print(chisq.test(abx_covid_only_waves$wave, abx_covid_only_waves[[item]]))
      
    }
    
    cat("\n==========================================================\n")
    print("USE OF COVID ANTIBIOTICS AMONG ALL PATIENTS!!")
    
    for (antibiotic in covid_antibiotics) {
      
      cat("\n==========================================================\n")
      print(antibiotic)
      
      tab <- table(abx_covid_only_waves$wave, abx_covid_only_waves[[antibiotic]])
      first_wave_percent <- round((tab[3]/(tab[3]+tab[1]))*100, 2)
      second_wave_percent <- round((tab[4]/(tab[4]+tab[2]))*100, 2)
      print(glue("{antibiotic} use in first wave: {first_wave_percent}"))
      print(glue("{antibiotic} use in second wave: {second_wave_percent}"))
      print(glue("Change is: {second_wave_percent - first_wave_percent}"))
      
      print(chisq.test(abx_covid_only_waves$wave, abx_covid_only_waves[[antibiotic]]))
      
    }
    
}

generate_forest_plots <- function(cleaned_abx_path) {

    abx_cleaned <- read.csv(cleaned_abx_path)

    # Subsetting for forest plots
    abx_df_morethan_50 <- abx_cleaned[abx_cleaned$name %in% more_than_50,]

    abx_df_northwest <- abx_df_morethan_50[abx_df_morethan_50$region=="northwest",]
    abx_df_east <- abx_df_morethan_50[abx_df_morethan_50$region=="east",]
    abx_df_south <- abx_df_morethan_50[abx_df_morethan_50$region=="south",]

    abx_df_wave1 <- abx_df_morethan_50[abx_df_morethan_50$wave==1,]
    abx_df_wave2 <- abx_df_morethan_50[abx_df_morethan_50$wave==2,]

    # Creating df for forest plots
    forest_50 <- get_forest_df(abx_df_morethan_50)

    forest_northwest <- get_forest_df(abx_df_northwest)
    forest_east <- get_forest_df(abx_df_east)
    forest_south <- get_forest_df(abx_df_south)

    forest_wave1 <- get_forest_df(abx_df_wave1)
    forest_wave2 <- get_forest_df(abx_df_wave2)

    save_forest_plot(forest_50, "forest_50")
    save_forest_plot(forest_northwest, "forest_northwest")
    save_forest_plot(forest_east, "forest_east")
    save_forest_plot(forest_south, "forest_south")
    save_forest_plot(forest_wave1, "forest_wave1")
    save_forest_plot(forest_wave2, "forest_wave2")
    
    cat("\n==========================================================\n")
    
    print("All forrest plots saved to data folder !!!")
    
}