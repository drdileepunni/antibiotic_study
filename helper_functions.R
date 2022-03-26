library(meta)

source("./cleaning_data.R")

##### DEFINING FUNCTIONS to replace bad names #####
replace_name <- function(df, before, after) {
  
  df$name <- gsub(before, after, df$name, fixed = TRUE)
  
  return(df)
  
}

add_columns <- function(abx_df) {

    #Creating COVID and ABX related new columns
    abx_df$covid_bin<-ifelse(abx_df$covid=="positive"| abx_df$covid=="suspected", 1, 0)
    abx_df$covid_bin<-as.numeric(abx_df$covid_bin)
    abx_df$abx_reserve <- ifelse(abx_df$name %in% reserve_group, 1, 0)
    abx_df$abx_watch <- ifelse(abx_df$name %in% watch_group, 1, 0)
    abx_df$abx_for_or <- ifelse(abx_df$name %in% abx_for_or, 1, 0)
    abx_df$abx_access <- ifelse(abx_df$name %in% access_group, 1, 0)
    abx_df$covid_specific_abx <- ifelse(abx_df$name %in% covid_specific_abx, 1,0)
    abx_df$Remdesivir <- ifelse(abx_df$name == "Remdesivir", 1,0)
    abx_df$Ivermectin <- ifelse(abx_df$name == "Ivermectin", 1,0)
    abx_df$Oseltamivir <- ifelse(abx_df$name == "Oseltamivir", 1,0)
    abx_df$Favipiravir <- ifelse(abx_df$name == "Favipiravir", 1,0)
    abx_df$Azithromycin <- ifelse(abx_df$name == "Azithromycin", 1,0)
    abx_df$Cefotaxime <- ifelse(abx_df$name == "Cefotaxime", 1,0)
    abx_df$Hydroxychloroquine <- ifelse(abx_df$name == "Hydroxychloroquine", 1,0)
    abx_df$piptaz <- ifelse(abx_df$name == "Piperacillin/Tazobactam", 1,0)
    abx_df$meropenem <- ifelse(abx_df$name == "Meropenem", 1,0)

    #Creating outcome related new columns
    abx_df$mortality <- ifelse(abx_df$discharge_disposition=="Death", 1, 0)
    abx_df$vent <-ifelse(abx_df$vented_hours > 0, 1, 0)
    abx_df$hfnc <-ifelse(abx_df$hfnc_hours >0,1,0)
    abx_df$niv <-ifelse(abx_df$niv_hours >0,1,0)
    abx_df$imv <-ifelse(abx_df$imv_hours >0,1,0)
    abx_df$mortality <- ifelse(abx_df$discharge_disposition=="Death", 1, 0)
    abx_df$vent <-ifelse(abx_df$vented_hours > 0, 1, 0)
    abx_df$thrombocytopenia <- ifelse(abx_df$min_Platelet < 150000, 1, 0)
    abx_df$leukocytosis <- ifelse(abx_df$max_Total.count > 11000, 1, 0)
    abx_df$acidosis <- ifelse(abx_df$min_pH < 7.35, 1, 0)
    abx_df$hypoxia <- ifelse(abx_df$min_PaO2 < 75, 1, 0)
    abx_df$hypercarbia <- ifelse(abx_df$max_PaCO2 >45, 1, 0)
    abx_df$hypocalcemia <- ifelse(abx_df$min_Calcium <8.5, 1, 0)
    abx_df$hyponatremia <- ifelse(abx_df$min_Na <135, 1, 0)
    abx_df$high_creat <- ifelse(abx_df$max_Creatinine > 1.3, 1, 0)

    #Creating a new column for region 
    abx_df$region <- with(abx_df, ifelse(abx_df$state_code %in% northwest, "northwest", 
                                         ifelse(abx_df$state_code %in% east, "east", 
                                                ifelse(abx_df$state_code %in% south, "south", "None"))))
    return(abx_df)
}

get_forest_df <- function(df) {
  
  df_or <- as.data.frame.matrix(table(df$name, df$covid_bin))
  df_or <- filter(df_or, df_or$`0`!=0)
  df_or <- filter(df_or, df_or$`1`!=0)
  region_non_covid <- table(df$covid_bin)[1]
  region_covid <- table(df$covid_bin)[2]
  df_or$covid_total <- region_covid
  df_or$non_covid_total <- region_non_covid
  names(df_or)[names(df_or) == '0'] <- 'non_covid'
  names(df_or)[names(df_or) == '1'] <- 'covid'
  return(df_or)
  
}

save_forest_plot <- function(forest_df, name) {

    m.bin <- metabin(covid,
                     covid_total,
                     non_covid,
                     non_covid_total,
                     data = forest_df,
                     studlab = paste(rownames(forest_df)),
                     comb.fixed = FALSE,
                     comb.random = TRUE,
                     method.tau = "DL",
                     hakn = TRUE,
                     prediction = TRUE,
                     incr = 0.1,
                     sm = "OR")

    h <- 1080*4
    png(file = glue("data/{name}.png"), height=2/3*h, width=h, res=300)
    forest_plot <- forest.meta(m.bin,
                                sortvar= TE,
                                xlim = c(0.01,5),
                                rightlabs = c("OR","95% CI","weight"),
                                leftlabs = c("Antibiotic", "Antibiotic use","Total","Antibiotic use","Total"),
                                lab.e = "COVID Group",
                                pooled.totals = TRUE,
                                #smlab = "",
                                squaresize = 0.5,
                                text.random = "Overall effect",
                                print.tau2 = TRUE,
                                col.diamond = "blue",
                                col.diamond.lines = "black",
                                col.predict = "black",
                                #print.I2.ci = TRUE
                                digits.tau2 = 2,
                                digits.sd = 2,
                                plotwidth = "5cm"
    )
    dev.off() 
    
}
