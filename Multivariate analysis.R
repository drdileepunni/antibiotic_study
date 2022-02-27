install.packages("gtsummary")
install.packages(sjPlot)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(gtsummary)
library(ResourceSelection)

 ##Doing UVA f/b MVA for mortality - replace with vent in the same code for vent UVA
abx_df %>%
  select(mortality, age,acidosis, hypercarbia, hypoxia, hfnc_hours,
           apache_score,leukocytosis, hypocalcemia, thrombocytopenia, hyponatremia,covid_bin ,high_creat , Remdesivir ,Oseltamivir, Favipiravir,Azithromycin ,Ivermectin ,abx_watch ,abx_for_or) %>%
  tbl_uvregression(
    method = glm,
    y = mortality,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 2)
  ) 
biochem_covid_abx <- glm(mortality ~ age  + acidosis+ hypercarbia+ hypoxia+ hfnc_hours+
                           apache_score + leukocytosis+ hypocalcemia+ thrombocytopenia+ hyponatremia +high_creat + Remdesivir +Oseltamivir+ Favipiravir+ Azithromycin +Ivermectin +abx_watch + abx_for_or +covid_bin,  data = abx_df, family = "binomial")
summary(biochem_covid_abx)
exp(coef(biochem_covid_abx))
coef(biochem_covid_abx)
tbl_regression(biochem_covid_abx, exponentiate = TRUE)

#Length of stay - COVID
covid_df_matched %>%
  select(los, age,acidosis, hypercarbia, hypoxia,
         apache_score,leukocytosis, hypocalcemia, hyponatremia ,high_creat , Remdesivir ,Oseltamivir, Favipiravir,Azithromycin ,Ivermectin ,abx_watch ,abx_for_or) %>%
  tbl_uvregression(
    method = lm,
    y = los,
    method.args = NONE,
    exponentiate = FALSE,
    pvalue_fun = ~style_pvalue(.x, digits = 2)
  ) 
table_4<- lm(los ~ age  + acidosis+ hypercarbia+ hypoxia+
                apache_score + leukocytosis+ hypocalcemia+ hyponatremia +high_creat + Remdesivir +Oseltamivir+ Favipiravir+ Azithromycin +Ivermectin +abx_watch + abx_for_or ,   data = covid_df_matched)
summary(table_4)
exp(coef(table_4))
coef(table_4)
tbl_regression(table_4, exponentiate = FALSE)

#ANy abx use - COVID cases- substitute individual abx
covid_df %>%
  select(Oseltamivir, age,acidosis, hypercarbia, hypoxia,
         apache_score,leukocytosis, hypocalcemia, hyponatremia ,high_creat) %>%
  tbl_uvregression(
    method = glm,
    y = Oseltamivir,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 2)
  ) 
biochem_covid_abx <- glm(Oseltamivir~ age  + acidosis+ hypercarbia+ hypoxia+
                           apache_score + leukocytosis+ hypocalcemia+ hyponatremia +high_creat,   data = covid_df, family = "binomial")
summary(biochem_covid_abx)
exp(coef(biochem_covid_abx))
coef(biochem_covid_abx)
tbl_regression(biochem_covid_abx, exponentiate = TRUE)

#watch group abx use - COVID cases
covid_df %>%
  select(Hydroxychloroquine, age,acidosis, hypercarbia, hypoxia,
         apache_score,leukocytosis, hyponatremia ,high_creat) %>%
  tbl_uvregression(
    method = glm,
    y = Hydroxychloroquine,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 2)
  ) 
biochem_covid_abx <- glm(Hydroxychloroquine ~ age  + acidosis+ hypercarbia+ hypoxia+
                           apache_score + leukocytosis+ hyponatremia +high_creat,   data = covid_df, family = "binomial")
summary(biochem_covid_abx)
exp(coef(biochem_covid_abx))
coef(biochem_covid_abx)
tbl_regression(biochem_covid_abx, exponentiate = TRUE)




########################################################
#####STEP 3.2 : Looking for risk factors of antibiotic use#########

#Defining function
do_univar_binom <- function(outcome, risk_factor) {
  model <- glm(outcome ~ risk_factor, family = "binomial", data = covid_df_matched)
  print(summary(model))
  exp(coef(model))
}
do_univar_cont <- function(outcome, risk_factor) {
  model <- glm(outcome ~ risk_factor, data = covid_df_matched)
  print(summary(model))
  exp(coef(model))
}


table<-do_univar_binom(covid_df_matched$mortality,covid_df_matched$age)
do_univar_binom(covid_df_matched$mortality,covid_df_matched$acidosis)
do_univar_binom(covid_df_matched$mortality,covid_df_matched$hypercarbia)
do_univar_binom(covid_df_matched$mortality,covid_df_matched$hypoxia)
do_univar_binom(covid_df_matched$mortality,covid_df_matched$hfnc_hours)
do_univar_binom(covid_df_matched$mortality,covid_df_matched$apache_score)
do_univar_binom(covid_df_matched$mortality,covid_df_matched$leukocytosis)
do_univar_binom(covid_df_matched$mortality,covid_df_matched$hypocalcemia)
do_univar_binom(covid_df_matched$mortality,covid_df_matched$thrombocytopenia)
do_univar_binom(covid_df_matched$mortality,covid_df_matched$hyponatremia)
do_univar_binom(covid_df_matched$mortality,covid_df_matched$high_creat)

do_univar_binom(covid_df_matched$mortality,covid_df_matched$abx_watch)
do_univar_binom(covid_df_matched$mortality,covid_df_matched$abx_for_or)
do_univar_binom(covid_df_matched$mortality,covid_df_matched$covid_specific_abx)
do_univar_binom(covid_df_matched$mortality,covid_df_matched$Remdesivir)
do_univar_binom(covid_df_matched$mortality,covid_df_matched$Favipiravir)
do_univar_binom(covid_df_matched$mortality,covid_df_matched$Azithromycin)
do_univar_binom(covid_df_matched$mortality,covid_df_matched$Cefotaxime)
do_univar_binom(covid_df_matched$mortality,covid_df_matched$Ivermectin)
do_univar_binom(covid_df_matched$mortality,covid_df_matched$Oseltamivir)

biochem_covid_abx <- glm(mortality ~ age  + acidosis+ hypercarbia+ hypoxia+ hfnc_hours+
                           apache_score + leukocytosis+ hypocalcemia+ thrombocytopenia+ hyponatremia +high_creat + Remdesivir +Oseltamivir+ Favipiravir+ Azithromycin +Ivermectin +abx_watch + abx_for_or,   data = covid_df_matched, family = "binomial")
summary(biochem_covid_abx)
exp(coef(biochem_covid_abx))
coef(biochem_covid_abx)
#Mortality/vent  - non COVID -UVA
do_univar_binom_2 <- function(outcome, risk_factor) {
  model_2 <- glm(outcome ~ risk_factor, family = "binomial", data = non_covid_df_matched)
  print(summary(model_2))
  (exp(coef(model_2)))
}
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$age)
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$acidosis)
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$hypercarbia)
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$hypoxia)
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$hfnc_hours)
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$apache_score)
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$leukocytosis)
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$hypocalcemia)
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$thrombocytopenia)
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$hyponatremia)
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$high_creat)

do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$abx_watch)
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$abx_for_or)
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$covid_specific_abx)
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$Remdesivir)
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$Favipiravir)
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$Azithromycin)
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$Cefotaxime)
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$Ivermectin)
do_univar_binom_2(non_covid_df_matched$vent,non_covid_df_matched$Oseltamivir)



#Mortality/vent - Non COVID MVA
mva_non_covid_df <- glm(vent ~ age  + acidosis+ hypercarbia+ hypoxia+ hfnc_hours+
                           apache_score + leukocytosis+ hypocalcemia+ thrombocytopenia+ hyponatremia +high_creat + Remdesivir +Oseltamivir+ Favipiravir+ Azithromycin +Ivermectin +abx_watch + abx_for_or,   data = non_covid_df_matched, family = "binomial")
summary(mva_non_covid_df)
exp(coef(mva_non_covid_df))

#Mortality/VENT _ Suspects UVA
do_univar_binom_3 <- function(outcome, risk_factor) {
  model_3 <- glm(outcome ~ risk_factor, family = "binomial", data = suspect_df_matched)
  print(summary(model_3))
  (exp(coef(model_3)))
}
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$age)
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$acidosis)
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$hypercarbia)
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$hypoxia)
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$hfnc_hours)
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$apache_score)
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$leukocytosis)
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$hypocalcemia)
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$thrombocytopenia)
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$hyponatremia)
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$high_creat)

do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$abx_watch)
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$abx_for_or)
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$covid_specific_abx)
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$Remdesivir)
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$Favipiravir)
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$Azithromycin)
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$Cefotaxime)
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$Ivermectin)
do_univar_binom_3(suspect_df_matched$vent,suspect_df_matched$Oseltamivir)
#Mortality/VENT -Suspects MVA
mva_covid_suspect_df <- glm(vent ~ age  + acidosis+ hypercarbia+ hypoxia+ hfnc_hours+
                          apache_score + leukocytosis+ hypocalcemia+ thrombocytopenia+ hyponatremia +high_creat + Remdesivir +Oseltamivir+ Favipiravir+ Azithromycin +Ivermectin +abx_watch + abx_for_or,   data = suspect_df_matched, family = "binomial")
summary(mva_covid_suspect_df)
exp(coef(mva_covid_suspect_df))

##Vent hours/ length of stay - COVID UVA
do_univar_cont <- function(outcome, risk_factor) {
  model <- glm(outcome ~ risk_factor, data = covid_df_matched)
  print(summary(model))
  exp(coef(model))
}

do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$age)
do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$acidosis)
do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$hypercarbia)
do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$hypoxia)
do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$hfnc_hours)
do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$apache_score)
do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$leukocytosis)
do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$hypocalcemia)
do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$thrombocytopenia)
do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$hyponatremia)
do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$high_creat)

do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$abx_watch)
do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$abx_for_or)
do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$covid_specific_abx)
do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$Remdesivir)
do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$Favipiravir)
do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$Azithromycin)
do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$Cefotaxime)
do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$Ivermectin)
do_univar_cont(covid_df_matched$vented_hours,covid_df_matched$Oseltamivir)

biochem_covid_abx <- glm(vented_hours ~ age  + acidosis+ hypercarbia+ hypoxia+ hfnc_hours+
                           apache_score + leukocytosis+ hypocalcemia+ thrombocytopenia+ hyponatremia +high_creat + Remdesivir +Oseltamivir+ Favipiravir+ Azithromycin +Ivermectin +abx_watch + abx_for_or,   data = covid_df_matched)
summary(biochem_covid_abx)
exp(coef(biochem_covid_abx))
coef(biochem_covid_abx)

#NON COVID Vent hours UVA and MVA
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$age)
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$acidosis)
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$hypercarbia)
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$hypoxia)
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$hfnc_hours)
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$apache_score)
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$leukocytosis)
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$hypocalcemia)
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$thrombocytopenia)
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$hyponatremia)
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$high_creat)

do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$abx_watch)
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$abx_for_or)
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$covid_specific_abx)
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$Remdesivir)
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$Favipiravir)
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$Azithromycin)
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$Cefotaxime)
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$Ivermectin)
do_univar_cont(non_covid_df_matched$vented_hours,non_covid_df_matched$Oseltamivir)

biochem_covid_abx <- glm(vented_hours ~ age  + acidosis+ hypercarbia+ hypoxia+ hfnc_hours+
                           apache_score + leukocytosis+ hypocalcemia+ thrombocytopenia+ hyponatremia +high_creat + Remdesivir +Oseltamivir+ Favipiravir+ Azithromycin +Ivermectin +abx_watch + abx_for_or,   data = non_covid_df_matched)
summary(biochem_covid_abx)
exp(coef(biochem_covid_abx))
coef(biochem_covid_abx)

  
  
