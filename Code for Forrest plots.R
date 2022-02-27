## Creating datasets needed to make Forrest plots

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
# All
abx_df_for_or <- abx_df[abx_df$name %in% abx_for_or,]
df_for_or <- get_forest_df(abx_df_for_or)
write.csv(df_for_or, 'df_for_or.csv')



###REGION WISE ANALYSIS FOR FOREST PLOT##
northwest_forest <- get_forest_df(abx_df_northwest)
east_forest <- get_forest_df(abx_df_east)
south_forest <- get_forest_df(abx_df_south)
write.csv(northwest_forest, "/Users/akhilaarya/Desktop/research/antibiotics/data/northwest_forest.csv")
write.csv(east_forest, "/Users/akhilaarya/Desktop/research/antibiotics/data/east_forest.csv")
write.csv(south_forest, "/Users/akhilaarya/Desktop/research/antibiotics/data/south_forest.csv")

northwest_forest <- get_forest_df(abx_df_northwest)
east_forest <- get_forest_df(abx_df_east)
south_forest <- get_forest_df(abx_df_south)
write.csv(northwest_forest, "/Users/akhilaarya/Desktop/research/data/northwest_forest.csv")
write.csv(east_forest, "/Users/akhilaarya/Desktop/research/data/east_forest.csv")
write.csv(south_forest, "/Users/akhilaarya/Desktop/research/data/south_forest.csv")
# More than 50 abx
more_than_50 <- c("Acyclovir", "Amikacin", "Amoxicillin/Clavulanic Acid", "Amphotericin B",
                  "Azithromycin","Caspofungin", "Cefepime",
                  "Cefepime + Sulbactam", "Cefixime","Cefoperazone/sulbactam", "Cefotaxime",
                  "Ceftriaxone", "Ceftriaxone/Tazobactam", "Cefuroxime",
                  "Ciprofloxacin", "Clarithromycin", "Clindamycin","Favipiravir", "Fluconazole",
                  "Imipenem/cilastatin","Levofloxacin", "Linezolid", "Meropenem","Ivermectin",
                  "Moxifloxacin","Nitrofurantoin","Oseltamivir","Piperacillin/Tazobactam", "Polymyxin", "Remdesivir", "Rifaximin", "Teicoplanin", "Tigecycline","Vancomycin", "Voriconazole")
abx_df_more_than_50 <- filter(abx_df_for_or, abx_df_for_or$name %in%more_than_50)
more_than_50_forest <- get_forest_df(abx_df_more_than_50)
write.csv(more_than_50_forest, "/Users/akhilaarya/Desktop/research/data/more_than_50_forest.csv")

# Suspect vs confirmed
abx_df$covid <- sub("^$", "negative", abx_df$covid)
abx_df_suspect <- filter(abx_df, abx_df$covid %in% c("suspected", "negative"))
abx_df_positive <- filter(abx_df, abx_df$covid %in% c("positive", "negative"))
suspect_forest <- get_forest_df(abx_df_suspect)
positive_forest <- get_forest_df(abx_df_positive)
write.csv(suspect_forest, "/Users/akhilaarya/Desktop/research/data/suspect_forest.csv")
write.csv(positive_forest, "/Users/akhilaarya/Desktop/research/data/positive_forest.csv")

##### WAVE WISE ANALYSIS #####

#1. 1st wave corresponding to March-December 2020 and 2nd wave to January to December 2021 
#2. 1st wave from March-Septemebr 2020 and 2nd wave from March -September 2021. Alternatively, you could also do March-Septemebr 2020 and February - August 2021.

# Step 1: change format of date_of_admission to timestamp
# Step 2: filter rows based on date of admission between specified dates --> new df
# Step 3: run get_forest_df to create forest df
# Step 4: Export as csv --> send to sitharah

# Converting datetime to factor
abx_df$date_of_admission <- strptime(x = as.character(abx_df$date_of_admission), format = "%Y-%m-%d %H:%M:%OS")

# MAR 2020 TO DEC 2020
DATE1 <- strptime(x = "2020-03-01", format = "%Y-%m-%d")
DATE2 <- strptime(x = "2020-12-31", format = "%Y-%m-%d") 
abx_mar_dec_20 <- abx_df[abx_df$date_of_admission >= DATE1 & abx_df$date_of_admission <= DATE2,]
abx_mar_dec_20_forest <- get_forest_df(abx_mar_dec_20)
write.csv(abx_mar_dec_20_forest, "/Users/akhilaarya/Desktop/research/data/abx_mar_dec_20_forest.csv")

#JAN 2021 to DEC 2021
DATE1 <- strptime(x = "2021-01-01", format = "%Y-%m-%d")
DATE2 <- strptime(x = "2021-12-31", format = "%Y-%m-%d") 
abx_jan_dec_21 <- abx_df[abx_df$date_of_admission >= DATE1 & abx_df$date_of_admission <= DATE2,]
abx_jan_dec_21_forest <- get_forest_df(abx_jan_dec_21)
write.csv(abx_jan_dec_21_forest, "/Users/akhilaarya/Desktop/research/data/abx_jan_dec_21_forest.csv")


#MAR 2020 to SEPT 2020
DATE1 <- strptime(x = "2020-03-01", format = "%Y-%m-%d")
DATE2 <- strptime(x = "2020-09-30", format = "%Y-%m-%d") 
abx_mar_sep_20 <- abx_df[abx_df$date_of_admission >= DATE1 & abx_df$date_of_admission <= DATE2,]
abx_mar_sep_20_forest <- get_forest_df(abx_mar_sep_20)
write.csv(abx_mar_sep_20_forest, "/Users/akhilaarya/Desktop/research/data/abx_mar_sep_20_forest.csv")


#MAR 2021 to SEP 2021
DATE3 <- strptime(x = "2021-03-01", format = "%Y-%m-%d")
DATE4 <- strptime(x = "2021-09-30", format = "%Y-%m-%d") 
abx_mar_sep_21 <- abx_df[abx_df$date_of_admission >= DATE3 & abx_df$date_of_admission <= DATE4,]
abx_mar_sep_21_forest <- get_forest_df(abx_mar_sep_21)
write.csv(abx_mar_sep_21_forest, "/Users/akhilaarya/Desktop/research/data/abx_mar_sep_21_forest.csv")
