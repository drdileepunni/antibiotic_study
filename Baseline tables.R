library(tableone)
install.packages("table1")
library(plotly)
library(ggplot2)
library(table1)

abx_df_count <- abx_df_cleaned %>% distinct(admission_id, .keep_all = TRUE)

## Changing 0 to NA so as to drop them out of mean/median calculation
abx_df_count$hfnc_hours[abx_df_count$hfnc_hours == 0] <- NA
abx_df_count$niv_hours[abx_df_count$niv_hours == 0] <- NA
abx_df_count$vented_hours[abx_df_count$vented_hours == 0] <-NA

##Inserting a new column for vented or not, hfnc or not niv or not
abx_df_count$vent <-ifelse(abx_df_count$vented_hours > 0, 1, 0)
abx_df_count$hfnc <-ifelse(abx_df_count$hfnc_hours >0,1,0)
abx_df_count$niv <-ifelse(abx_df_count$niv_hours >0,1,0)
abx_df_count$imv <- ifelse(abx_df_count$imv_hours >0,1,0)
vent_clean <- filter(abx_df_count, (abx_df_count$vented_hours >=0) & (abx_df_count$vented_hours<=300))


abx_df_count$vent <- 
  factor(abx_df_count$vent, levels=c(1,0),
         labels=c("Vented", 
                  "Not vented"))
abx_df_count$abx_watch <- 
  factor(abx_df_count$abx_watch, levels=c(1,0),
         labels=c("Yes", 
                  "No"))
abx_df_count$abx_reserve <- 
  factor(abx_df_count$abx_reserve, levels=c(1,0),
         labels=c("Yes", 
                  "No"))
abx_df_count$covid_specific_abx <- 
  factor(abx_df_count$covid_specific_abx, levels=c(1,0),
         labels=c("Yes", 
                  "No"))
abx_df_count$hfnc <-factor(abx_df_count$hfnc, levels = c(1,0),
                     labels = c("Yes", "No"))
abx_df_count$niv <-factor(abx_df_count$niv, levels = c(1,0),
                     labels = c("Yes", "No"))
abx_df_count$imv <- factor(abx_df_count$imv, levels = c(1,0), labels = c("Yes", "No"))

abx_df_count$covid <-factor(abx_df_count$covid, levels=c("", "positive", "suspected"),labels=c("COVID Negative", "COVID Positve", "COVID Suspected"))


label(abx_df_count$gender)       <- "Gender"
label(abx_df_count$age)       <- "Age"
label(abx_df_count$discharge_disposition)     <- "Outcome"
label(abx_df_count$apache_score) <- "APACHE score"
label(abx_df_count$vent)       <- "Ventilation status"
label(abx_df_count$abx_watch)       <- "Recieved watch group antibiotics"
label(abx_df_count$abx_reserve)     <- "Recieved reserve group antibiotics"
label(abx_df_count$niv) <- "NIV use"
label(abx_df_count$hfnc) <- "HFNC use"
label(abx_df_count$imv_hours) <- "IMV hours"
label(abx_df_count$niv_hours) <- "NIV hours"
label(abx_df_count$hfnc_hours) <- "HFNC hours"
label(abx_df_count$vented_hours) <- "Vented hours"
summary_table <- table1::table1(~ gender + age + discharge_disposition + apache_score +  abx_watch + abx_reserve +covid_specific_abx+  vent + hfnc + niv + imv + hfnc_hours + niv_hours + vented_hours| covid, data=abx_df_count )
print(summary_table)

units(abx_df_count$age)       <- "years"

abx_df_count_neg <- filter(abx_df_count, abx_df_count$covid_bin == 0)
abx_df_count_pos <- filter(abx_df_count, abx_df_count$covid == "positive")
abx_df_count_sus <- filter(abx_df_count, abx_df_count$covid == "suspected")

#Table to find frequency of antibiotic prescription in COVID(table2)/overall(table3)
table_2 <- table1::table1(~ name  | covid, data=abx_mar_sep_21 )
print(table_2)
table_3 <- table1::table1(~ name  | covid, data=abx_df_cleaned )
print(table_3)
##Next table - wave wise abx in vented vs non vented COVID
mar_sep_20_cov <- filter(abx_mar_sep_20, abx_mar_sep_20$covid_bin == 1)
mar_sep_21_cov <- filter(abx_mar_sep_21, abx_mar_sep_21$covid_bin == 1)

mar_sep_20_cov$vent <- 
  factor(mar_sep_20_cov$vent, levels=c(1,0),
         labels=c("Vented", 
                  "Not vented"))

mar_sep_20_cov$abx_for_or <- 
  factor(mar_sep_20_cov$abx_for_or, levels=c(1,0),
         labels=c("Yes", 
                  "No"))
table_4 <- table1::table1(~ abx_for_or  | vent, data=mar_sep_20_cov )
print(table_4)

mar_sep_21_cov$vent <- 
  factor(mar_sep_21_cov$vent, levels=c(1,0),
         labels=c("Vented", 
                  "Not vented"))

mar_sep_21_cov$abx_for_or <- 
  factor(mar_sep_21_cov$abx_for_or, levels=c(1,0),
         labels=c("Yes", 
                  "No"))
table_5 <- table1::table1(~ name  | vent, data=covid_df )
print(table_5)
##Chi squares for results of histogram trends
chisq.test(mar_sep_20_cov$abx_for_or, mar_sep_20_cov$vent)
chisq.test(abx_mar_sep_20$covid_bin, abx_mar_sep_20$abx_for_or)
       