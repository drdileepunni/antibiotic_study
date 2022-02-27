library(dplyr)
library(ggplot2)
library(MatchIt)
f <- file.choose()
abx_df<-read.csv(f)



##### DEFINING FUNCTIONS to replace bad names #####
replace_name <- function(df, before, after) {
  
  df$name <- gsub(before, after, df$name, fixed = TRUE)
  
  return(df)
  
}

##### CLEANING #####

# Replacing bad names
abx_df <- replace_name(abx_df, 'Aciclovir', 'Acyclovir')
abx_df <- replace_name(abx_df, 'Colistimethate Sodium', 'Polymyxin')
abx_df <- replace_name(abx_df, 'Amoxicillin/potassium', 'Amoxicillin')
abx_df <- replace_name(abx_df, 'Cefpodoxmine Proxetil', 'Cefpodoxime')
abx_df <- replace_name(abx_df, 'Polymyxin B', 'Polymyxin')
abx_df <- replace_name(abx_df, 'Polymixin B', 'Polymyxin')
abx_df <- replace_name(abx_df, 'Colistin', 'Polymyxin')
abx_df <- replace_name(abx_df, 'Amphotericin B (Liposomal )', 'Amphotericin B')
abx_df <- replace_name(abx_df,'Valaciclovir', 'Valacyclovir')
abx_df <- replace_name(abx_df,'Valacyclovir ( Valacyclovir) ', 'Valacyclovir')
abx_df <- replace_name(abx_df,'Cefipime', 'Cefepime')
abx_df <- replace_name(abx_df,'Ceftazidime + Tazobactum', 'Ceftazidime + Tazobactam')
abx_df <- replace_name(abx_df,'Meropenem ', 'Meropenem')
abx_df <- replace_name(abx_df,'Capsofungin ', 'Caspofungin')
abx_df <- replace_name(abx_df,'Gentamycin', 'Gentamicin')
abx_df <- replace_name(abx_df,'Oseltamivir Phosphate', 'Oseltamivir')
abx_df <- replace_name(abx_df, 'Hydroxychloroquine (HCQS)', 'Hydroxychloroquine')

#to recode variables without NAs
abx_df$age_good<-complete.cases(abx_df$age)
#age_good<-complete.cases(age)
# is.na(age_good)

abx_df$gender_good<-complete.cases(abx_df$gender)
#gender_good<-complete.cases(gender)
# is.na(gender_good)

abx_df$apache_score_good<-complete.cases(abx_df$apache_score)
#apache_score_good<-complete.cases(apache_score)

#Creating COVID bin as column
abx_df$covid_bin<-ifelse(abx_df$covid=="positive"| abx_df$covid=="suspected", 1, 0)
abx_df$covid_bin<-as.numeric(abx_df$covid_bin)
abx_df_matched$suspect<-ifelse(abx_df_matched$covid=="suspected", 1, 0)

##### DEFINING ANTIBIOTIC GROUPS #####

reserve_group <- c("Fosfomycin", "Linezolid", "Tigecycline","Daptomycin","Cefipime","Cefepime + Sulbactam","Cefepime + Tazobactam")
watch_group <- c("Ciprofloxacin", "Levofloxacin","Moxifloxacin","Norfloxacin", "Ofloxacin",
                 "Prulifloxacin","Levofloxacin","Ciprofloxacin","Cefixime","Ceftriaxone","Ceftriaxone/Sulbactam",
                 "Ceftriaxone/Tazobactam","Cefotaxime",
                 "Ceftazidime","Ceftazidime + Tazobactam","Ceftazidime/Avibactam","Azithromycin","Clarithromycin","Erythromycin",
                 "Vancomycin","Teicoplanin","Piperacillin/Tazobactam","Meropenem","Meropenem + Tazobactam","Meropenem/Sulbactam","Ertapenem","Faropenem",
                 "Doripenem")
abx_for_or <- c("Acyclovir", "Amikacin", "Amoxicillin", "Amoxicillin/Clavulanic Acid", "Amphotericin B",
                "Ampicillin", "Azithromycin", "Benzyl Pencillin", "Caspofungin", "Cefepime",
                "Cefepime + Sulbactam", "Cefixime", "Cefoperazone","Cefoperazone/sulbactam", "Cefotaxime", "Cefpodoxime",
                "Ceftazidime","Ceftazidime/Avibactam", "Ceftriaxone", "Ceftriaxone/Sulbactam", "Ceftriaxone/Tazobactam", "Cefuroxime",
                "Ciprofloxacin", "Clarithromycin", "Clindamycin", "Ertapenem", "Favipiravir", "Fluconazole", "Fosfomycin", "Gentamicin",
                "Imipenem/cilastatin", "Isavuconazole", "Itraconazole","Ivermectin", "Levofloxacin", "Linezolid", "Meropenem", "Meropenem/Sulbactam",
                " Micafungin", "Moxifloxacin", "Norfloxacin","Nitrofurantoin", "Norfloxacin", "Ofloxacin", "Oseltamivir","Piperacillin",
                "Piperacillin/Tazobactam", "Polymyxin", "Posaconazole", "Remdesivir", "Rifaximin", "Teicoplanin", "Tigecycline",
                "Vancomycin", "Voriconazole")
covid_specific_abx <- c("Cefotaxime","Azithromycin","Remdesivir","Favipiravir","Oseltamivir","Ivermectin")

more_than_50 <- c("Acyclovir", "Amikacin", "Amoxicillin/Clavulanic Acid", "Amphotericin B",
                  "Azithromycin","Caspofungin", "Cefepime",
                  "Cefepime + Sulbactam", "Cefixime","Cefoperazone/sulbactam", "Cefotaxime",
                  "Ceftriaxone", "Ceftriaxone/Tazobactam", "Cefuroxime",
                  "Ciprofloxacin", "Clarithromycin", "Clindamycin","Favipiravir", "Fluconazole",
                  "Imipenem/cilastatin","Levofloxacin", "Linezolid", "Meropenem","Ivermectin",
                  "Moxifloxacin","Nitrofurantoin","Oseltamivir","Piperacillin/Tazobactam", "Polymyxin", "Remdesivir", "Rifaximin", "Teicoplanin", "Tigecycline","Vancomycin", "Voriconazole")

access_group <- c('Amikacin','Amoxicillin','Amoxicillin/clavulanic acid', 'Ampicillin','Ampicillin sulbactam','Bacampicillin','Benzathine benzylpenicillin',	
                  'Benzylpenicillin','Cefacetrile','Cefadroxil','Cefalexin','Cefalotin','Cefapirin','Cefatrizine','Cefazedone','Cefazolin','Cefradine','Cefroxadine',
                  'Ceftezole','Chloramphenicol','Clindamycin','Clometocillin','Cloxacillin','Dicloxacillin','Doxycycline','Flucloxacillin','Gentamicin','Mecillinam',
                  'Metronidazole','Metronidazole','Nafcillin','Nitrofurantoin','Oxacillin','Penamecillin','Phenoxymethylpenicillin','Pivampicillin','Pivmecillinam',
                  'Procaine benzylpenicillin',	'Spectinomycin','Tetracycline','Thiamphenicol','Trimethoprim')
abx_df$abx_reserve <- ifelse(abx_df$name %in% reserve_group, 1, 0)
table(abx_df$abx_reserve, abx_df$covid_bin)

abx_df$abx_watch <- ifelse(abx_df$name %in% watch_group, 1, 0)
table(abx_df$abx_watch, abx_df$covid_bin)

abx_df$abx_for_or <- ifelse(abx_df$name %in% abx_for_or, 1, 0)
table(abx_df$abx_for_or, abx_df$covid_bin)

abx_df$abx_access <- ifelse(abx_df$name %in% access_group, 1, 0)
table(abx_df$abx_access, abx_df$covid_bin)


abx_df$covid_specific_abx <- ifelse(abx_df$name %in% covid_specific_abx, 1,0)
abx_df_more_than_50 <- filter(abx_df_for_or, abx_df_for_or$name %in%more_than_50)

#Filtering out entries with negative vent hours
abx_df <- filter(abx_df, vented_hours >= 0)
#Defining some variables
abx_df$mortality <- ifelse(abx_df$discharge_disposition=="Death", 1, 0)
abx_df$vent <-ifelse(abx_df$vented_hours > 0, 1, 0)
abx_df$hfnc <-ifelse(abx_df$hfnc_hours >0,1,0)
abx_df$niv <-ifelse(abx_df$niv_hours >0,1,0)
abx_df$Remdesivir <- ifelse(abx_df$name == "Remdesivir", 1,0)
abx_df$Ivermectin <- ifelse(abx_df$name == "Ivermectin", 1,0)
abx_df$Oseltamivir <- ifelse(abx_df$name == "Oseltamivir", 1,0)
abx_df$Favipiravir <- ifelse(abx_df$name == "Favipiravir", 1,0)
abx_df$Azithromycin <- ifelse(abx_df$name == "Azithromycin", 1,0)
abx_df$Cefotaxime <- ifelse(abx_df$name == "Cefotaxime", 1,0)
abx_df$Hydroxychloroquine <- ifelse(abx_df$name == "Hydroxychloroquine", 1,0)
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

# Defining regions
northwest <- c("PB", "HR", "LA", "UP", "MP", "GJ")
east <- c("AS", "BR", "JH", "WB", "OD")
south <- c("AP", "KA", "KL", "TN", "MH")

#Creating a new column for region 
abx_df$region <- with(abx_df, ifelse(abx_df$state_code %in% northwest, "northwest", 
                                     ifelse(abx_df$state_code %in% east, "east", 
                                            ifelse(abx_df$state_code %in% south, "south", "None"))))
#Creating region wise dataset
abx_df_northwest <- filter(abx_df, (region=="northwest"))
abx_df_east <- filter(abx_df, (region=="east"))
abx_df_south <- filter(abx_df, (region=="south"))


#Removing duplicates based on admission id to get patient count
abx_df_count <- abx_df %>% distinct(admission_id, .keep_all = TRUE)

# Saving cleaned dataset
abx_df_cleaned <- abx_df
write.csv(abx_df_cleaned, "/Users/akhilaarya/Desktop/research/antibiotics/data/abx_df_cleaned.csv")

#Creating covid only dataset
covid_df <-filter(abx_df,(covid_bin == 1)) 
non_covid_df <- filter(abx_df_cleaned,(covid_bin==0))

###MATCHING###


#matching region wise for second analysis.
m.out2 <- matchit(covid_bin~age_good+gender_good+apache_score_good+ region,data = abx_df)
m.out2
s.out<-summary(m.out2, un=FALSE, standardize = TRUE)
s.out
#create new df with matched data (second matched)
abx_df_matched <- match.data(m.out2)
# write.csv(abx_df_matched, "/Users/akhilaarya/Desktop/research/data/abx_df_matched.csv")

## Making covid only dataset to run LRC of watch abx
covid_df_matched <- filter(abx_df_matched,(covid_bin == 1)) 
non_covid_df_matched <- filter(abx_df_matched,(covid_bin ==0))
suspect_df_matched <- filter(abx_df_matched,(suspect ==1))


