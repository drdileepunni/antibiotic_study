# Antibiotic_study
All R code used for COVID19 antibiotic analysis

## Details
All codes except forrest plot, UVA, MVA are compiled in these scripts

## Instructions to run
* You need to have access to two datasets to successfully run these scripts. Place these two csv files to a folder named data in your root directory
* make root directory (directory with all the R scripts and data folder) your working directory ```setwd("path/to/root/dir")```
* You'll need to install all requirement packages, this includes
```
tableone
tabel1
meta
dplyr
ggplot2
hash
glue
rvest
dplyr
```
* Load all the three scripts using 
```
source("./cleaning_data.R")
source("./helper_functions.R")
source("./aggregation_functions.R")
```
* Define paths to two datasets as all_antibiotics_path and all_admissions_path
* Run the following functions
```
cleaned_abx_path <- clean_abx_dataset(all_antibiotics_path)
get_summary_table(cleaned_abx_path)
get_admission_related_details(all_admissions_path)
get_abx_related_details(cleaned_abx_path)
generate_forest_plots(cleaned_abx_path)
```
* At the end of the runs, your data folder will all the interim datasets in it. abx_df_cleaned.csv is the dataset to use for UVA, MVA and making plots.
