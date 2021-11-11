
#--------------------------------------------------------------------------- -
#--------------------------------------------------------------------------- -
#--------------------------------------------------------------------------- -
#----------------------- PREAMBLE ------------------------------------------ -

# Brief purpose of code:

#  Create subject-level demographic covariates for each patient
#  in target population

# Code author: [ENTER NAME]
# Lead analyst: [ENTER NAME]
# Project lead: [ENTER NAME]
# Research lead: [ENTER NAME]
# Additional supp: [ENTER NAMES]
# Date file created: [ENTER]


# Checklist: MUST BE COMPLETED BY CODE AUTHOR

#  [ ] Self QC (line by line code review)
#  [ ] Code is sufficiently commented
#  [ ] Code is organized logically and flows well
#  [ ] Script runs from start to finish without error
#  [ ] Script runs from start to finish without critical warning messages
#  [ ] Code blocks have been spotchecked and run without error or warning messages

# Checklist for DATA MANAGEMENT:

#  [ ] Column variables (ie, covariates, outcomes, or ancillary variables)
#      have been spotchecked for face validity
#  [ ] ONGOING LIST


# Checklist for ANALYSIS:
# [ ] Model covariates have sufficient samples within each category
# [ ] Parameter estimates are reasonable (check magnitudes)
# [ ] Estimates of precision (ie, standard errors) are reasonable (check for unboundedness)
# [ ] ONGOING LIST


# This checklist is a preliminary and ultimately living document.
# If you think of something that should be modified or added, please
# bring your ideas to Paul Ekwaru or Paul Spin

# INSPIRATION:

# Resources for Reproducible Research
# https://bookdown.org/aschmi11/RESMHandbook/data-preparation-and-cleaning-in-r.html
# https://geanders.github.io/RProgrammingForResearch/
# https://itsmecevi.github.io/data-cleaning-101/#



#--------------------------------------------------------------------------- -
#--------------------------------------------------------------------------- -
#--------------------------------------------------------------------------- -



#--------------------------------------------------------------------------- -
#--------------------------------------------------------------------------- -
# 1. File Setup --------------------------------------------------------------

# 1.1 Libraries --------------------------------------------------------------
library(dplyr) # Data wrangling utilities
library(purrr) # Functional programming utilities
library(survival) # Survival models
library(openxls) # Excel utilities
library(survminer) # Survival/Cox Model Post-Estimation Tools
library(ggplot2) # Plotting
library(readr) # Utility for loading CSVs, Tab delimited files, text files
library(haven) # Library for opening SAS, Stata, SPSS files
library(lubridate) # Wrangling dates in R


# 1.2 Custom functions -------------------------------------------------------
rfuns <- list.files("R/")
lapply(rfuns, function(x) source(paste0("R/",x))) # Loads functions to memory


# 1.3. Load data dependencies ------------------------------------------------

# Load all data sources required to derive fields of interest

# Note that this file must load a data file into memory. The data file must be stored as
# a data.frame or tibble  named study_dat

# If the cohort size is too large to store in memory (this is rare) then see
# Paul Spin or Paul Ekwardu for further guidance.

# Failure to load a data file with this name result in an error warning being issued when
# running the script and you will be forced to terminate the code run


# read.csv()          # Read CSV file
# read_excel()        # Read both xls and xlsx files and detects the format from the extension.
# read_sheets()       # Read worksheets of xls or xlsx file
# haven::read_sas()   # Load SAS data file
# haven::read_xpt()   # Load XPT file
# load()              # Load R data files

# 1.4. Check for study level variable ----------------------------------------
if(exists("study_dat")==FALSE) {
  stop("study_dat is not stored in memory. Your study-level variables may
       be stored under a different name. If so, please assign it to an object
       named study_dat")
}

# 1.5 Extract unique subject ID and index date -------------------------------
# Each covariate will be defined and added to this data set iteratively.
# The resulting datasets will be merged and then added to the study-level
# dataset

# Example
# If you have two covariates (sex, age), then two temporary data files
# will be created, one for each covariate. Once complete, these two files
# will be merged together, and the resulting merged file will itself
# be merged into the study-level file.

dat <- study_dat %>%
  dplyr::select(usubjid, index_date)


#--------------------------------------------------------------------------- -
#--------------------------------------------------------------------------- -
# 2. Demographics           --------------------------------------------------
#--------------------------------------------------------------------------- -
#--------------------------------------------------------------------------- -

# Common variables
# [1] Age at index. Requires date of birth and index date
# --> 2 variables:
#     - Numeric - integer in years
# --> Age categories at index. Requires age at index
# --> Variable cut points (adapt as needed)
# --> <18 years, 18-25 years, 25-34 years, 35-44 years, 45-54 years,
#     55-64 years, 65-74 years, 75+ years
# --> 2 variables:
#     - Factor
#     - Character
# [2] Sex. Converted to one binary variable: Female (1 = Female; 0 = Male)
# --> 3  variables:
#     - Numeric - integer
#       - binary variable: Female (1 = Female; 0 = Male)
#     - Factor
#     - Character
# [3] Geography (Alberta)
# --> 2 variables:
#     - Factor
#     - Character
# [4] [OTHER - specify]


# 2.1. Bespoke functions  -----------------------------------------------------
# Convert character variable to factor variable
bespoke_char_to_factor <- function(x){
  if(class(x)=="character") {
    x <- factor(x)
  }

  return(x)
}


# 2.2. Process date variables  -------------------------------------------------
# Example:
# source_file$date <- strptime(
#   sourcefile$date,
#   format = "%m/%d%Y" # check this is accurate format
# )

# Repeat for other date variables such as date of birth



# 2.3. Age  -------------------------------------------------------------------
dat1 <- source_file %>%
  dplyr::filter(
    # Keep subjects in study file
    usubjid %in% study_dat$usubjid
  ) %>%
  #... Select subject identifier and dob
  dplyr::select(
    usubjid, dob=date_of_birth
  ) %>%
  #... Merge into study data
  dplyr::right_join(
    study_dat
  ) %>%
  dplyr::mutate(
    age = floor(as.numeric(difftime(
      dob,
      index_date,
      units = "days"
    )) / 365.25 ),
    agecat_factor = cut(
      age,
      breaks = c(0, 18, 25, 35, 45, 55, 65, 75, Inf),
      right = FALSE,
      labels = c(
        "<18", "18-24", "25-34", "35-44", "45-54",
        "55-64", "65-74", ">= 75"
      )
    ),
    agecat_character = as.character(agecat_factor)
  )


# 2.4. Sex  -------------------------------------------------------------------
# Modify as needed
dat2 <- source_file %>%
  dplyr::filter(
    # Keep subjects in study file
    usubjid %in% study_dat$usubjid
  ) %>%
  #... Select subject identifier and dob
  dplyr::select(
    usubjid, sex
  )


# 2.5. Geography  -------------------------------------------------------------

# [ADD CODE]


# 2.6. [Other variables...]  --------------------------------------------------



#--------------------------------------------------------------------------- -
#--------------------------------------------------------------------------- -
# 3. TIDY DATA SET          --------------------------------------------------
#--------------------------------------------------------------------------- -
#--------------------------------------------------------------------------- -

# 3.1. Unify data ------------------------------------------------------------

# This section compiles a single data frame from each code chunk of Section 2

# Note, this file should only include the following fields

#  [ ] Unique subject ID (usubjid)
#  [ ] Index date (index_date)
#  [ ] Date of birth
#  [ ] Age variables including continuous, factors, and strings
#  [ ] Sex
#  [ ] Geography as factors and strings
#  [ ] Other variables [Specify]


# dat <- study_dat %>%
# # Join study data with covariates
# dplyr::left_join(
#   purrr::reduce(
#     list(dat1, dat2, dat3), # Add/Remove as needed
#     left_join
#   )
# )


# Confirm only those subgroups listed at beginning of Section 3 are in data set
colnames(dat)


# 3.4. Data validation  ------------------------------------------------------

# Simple data checks
#head(dat) # First few rows
#summary(dat) # summary table of rows
# How many patients in data set
# dat %>%
#   dplyr::pull(usubjid) %>%
#   unique %>% length

# How many patients with missing data for age
sum(is.na(dat$age))

# Tally counts by age and sex

dat %>%
  dplyr::group_by(
    age_cat, sex
  ) %>%
  dplyr::tally()



# Interactive checking can be done using the R package `validate`
# or other packages
# https://cran.r-project.org/web/packages/validate/vignettes/cookbook.html

# Example of basic use of validation package
library(validate)
data(cars)
rules <- validator(speed >= 0
                 , dist >= 0
                 , speed/dist <= 1.5
                 , cor(speed, dist)>=0.2)
conf <- confront(cars, rules)
summary(conf)


# 3.4. Label data ------------------------------------------------------------
# 3.4.1 Label variables/columns ----------------------------------------------
# labelled::var_label(dat) <- list(
#   usubjid = "Unique patient identifier"
#   index_date = "First date patient satifies all eligibility criteria for study"
#   [Add additional labels]
# )


#--------------------------------------------------------------------------- -
#--------------------------------------------------------------------------- -
# 4. EXPORT                 --------------------------------------------------
#--------------------------------------------------------------------------- -
#--------------------------------------------------------------------------- -

# [code block for exporting to RData or SAS]

# If exporting to SAS, confirm that variable labels are preserved
# by reloading the SAS file into R and confirming the variable labels are still present

