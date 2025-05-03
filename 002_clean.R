#Script----
# Name: 002_clean.R
# Author: Parushya, Aarthi
# Purpose: Cleaning male, female and non-surveyed datasets
# Date Created: 2021/11/24


#Check packages----

# Checking if packages are installed and installing
# check.packages function: install and load multiple R packages.
# Found this function here: https://gist.github.com/smithdanielle/9913897 on 2019/06/17
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
check.packages <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}

# Check if packages are installed and loaded:
packages <- c("janitor", "tidyverse", "utils", "here","lubridate","Hmisc","stringr")
check.packages(packages)

# Setting Directories and creating subfolders----

# Sub-directories
rawdata <- paste0(here::here(), "/data/raw/main_raw/")
scripts <- paste0(here::here(), "/data/scripts/main_scripts/")
cleandata <- paste0(here::here(), "/data/clean/main_clean/")

# Loading data----
# Loading of datasets is being done in individual clean scripts
# Loading functions----

source(paste0(scripts, "Functions.R"))

# Sourcing separate cleaning scripts----

# Cleaning the female survey: 
source(paste0(scripts, "clean_female.R")) 

# Cleaning the male survey:
source(paste0(scripts, "clean_male.R"))

# Saving data----
# Being done in Individual cleaning scripts
save(df_female, df_male, df_assets_rural, df_assets_rural_wt, df_assets_urban, df_assets_urban_wt,  flfs_wts_mf, file=paste0(here("data/clean/main_clean/"),"SurveyCleanData.RData"))


