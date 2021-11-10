#' Create project structure
#'
#'
#' @param dir character denoting path of directory where the project will be created
#'
#' @export
#'
create_project <- function(dir, replace = FALSE) {

  # 1. Errors   ----------------------------------------------

  # ... Issue errors if directory already exists
  if (replace == TRUE) {
    warning("If directory exists, then it will be overwritten.")

  } else {
    if (dir.exists(dir)) stop("Directory already exists")

  }

  # 2. Warnings  ---------------------------------------------


  # 3 Initialize structure -----------------------------------
  # 3.1 Create initial directory -----------------------------

  # ... Create directory
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)


  # 3.2 Create subdirectories --------------------------------
  subdir <- list(
    fun = c("R"),
    data = c("data", "1_raw", "2_intermediate", "3_analysis", "9_ancillary"),
    scripts = c("inst", "1_clean", "2_describe", "3_model", "4_validate", "5_report", "9_adhoc"),
    docs = c("docs"),
    confidential_docs = c("confidential_docs"),
    rmd = c("rmd", "1_draft_reports", "2_final_reports", "9_adhoc_reports")
  )

  lapply(
    subdir,
    function(x) {
      # Create directory one level into root

      update_path <- paste(
        dir,
        x[1],
        sep = "/"
      )

      dir.create(update_path, recursive = TRUE)

      # Create directories two levels down

      if (length(x)>1) {
        lapply(2:length(x), function(y){

          update_path <- paste(
            dir,
            x[1],
            x[y],
            sep = "/"
          )

          dir.create(update_path, recursive = TRUE)

        })
      }

    }
  )
}


#' Create R function
#'
#'
#' @param dir character denoting path of directory where the project will be created
#'
#' @export
#'

template_rfunction <- function(target_file = "99_example-template.R"){

  # 1. Connect file --------------------------------------
  file_conn <- base::file(paste0("R/", target_file))

  # 2 Contents of file -----------------------------------

  lines <-'
#\' [Enter description of function]
#\'
#\'
#\' @param x1 [Enter description of 1st argument of function]
#\' @param x2 [Enter description of 2nd argument of function]
#\'
#\'
#\' @export [Remove this line if you do not intend to create a package]

myfun <- function(
          x1,
          x2 #, Uncomment to add additional arguments
          #[...] Additional arguments
) {

    # 1. Errors ------------------------------------------------------------------------

    #... RECOMMENDED

    if(cond) {
      stop("Error message")
    }


    # 2. Warnings ----------------------------------------------------------------------

    #... RECOMMENDED


    if(cond) {
      warning("warning message")
    }

    # 3. Required packages  ------------------------------------------------------------

    #... OPTIONAL


    # 4. Code block 1       ------------------------------------------------------------



    # 5. Code block 2       ------------------------------------------------------------



    #... ADDITIONAL CODE



    # RETURN OUTPUT

    return(x)

}'

  # Write lines to file connection

  writeLines(text = lines, con = file_conn)

  # Close file connection
  close(file_conn)

  # Open user-facing file
  file.edit(paste0("R/", target_file))


}



#' Create Data Cleaning Scripts
#'
#'
#' @param dir character denoting path of directory where the project will be created
#'
#' @export
#'

template_rfunction <- function(target_file = "inst/1_clean"){

  # 1. Define index date ---------------------------------

  # 1.1. File connection ---------------------------------
  file_conn <- base::file(paste(target_file, "01_index"))

  # 1.2 Contents of file ---------------------------------

  lines1 <-'
#\' [Enter description of function]
#\'
#\'
#\' @param x1 [Enter description of 1st argument of function]
#\' @param x2 [Enter description of 2nd argument of function]
#\'
#\'
#\' @export [Remove this line if you do not intend to create a package]

myfun <- function(
          x1,
          x2 #, Uncomment to add additional arguments
          #[...] Additional arguments
) {

    # 1. Errors ------------------------------------------------------------------------

    #... RECOMMENDED

    if(cond) {
      stop("Error message")
    }


    # 2. Warnings ----------------------------------------------------------------------

    #... RECOMMENDED


    if(cond) {
      warning("warning message")
    }

    # 3. Required packages  ------------------------------------------------------------

    #... OPTIONAL


    # 4. Code block 1       ------------------------------------------------------------



    # 5. Code block 2       ------------------------------------------------------------



    #... ADDITIONAL CODE



    # RETURN OUTPUT

    return(x)

}'

  # Write lines to file connection

  writeLines(text = lines, con = file_conn)

  # Close file connection
  close(file_conn)

  # Open user-facing file
  file.edit(paste0("R/", target_file))


}



script_preamble <- function(){


line1 <-
'
#--------------------------------------------------------------------------- -
#--------------------------------------------------------------------------- -
#--------------------------------------------------------------------------- -
#----------------------- PREAMBLE ------------------------------------------ -

# Brief purpose of code:

  [ Text]

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


# 1.2 Custom functions -------------------------------------------------------
rfuns <- list.files("R/")
lapply(rfuns, function(x) source(paste0("R/",x))) # Loads functions to memory


# 1.3. Load data dependencies ------------------------------------------------
# read.csv()          # Read CSV file
# read_excel()        # Read both xls and xlsx files and detects the format from the extension.
# read_sheets()       # Read worksheets of xls or xlsx file
# haven::read_sas()   # Load SAS data file
# haven::read_xpt()   # Load XPT file
# load()              # Load R data files



#--------------------------------------------------------------------------- -
#--------------------------------------------------------------------------- -
# 2. COHORT IDENTIFICATION  --------------------------------------------------
#--------------------------------------------------------------------------- -
#--------------------------------------------------------------------------- -

# 2.1. Study Level File  -----------------------------------------------------

# Key fields
# [1] Subject ID
# [2] Create Indicator Variables for Each Inclusion/Exclusion Criteria
#     to denote whether patient satisfies a given criteria
# [3] Subgroup or population of interest flags


# 2.2. Overall population

d1 <- raw %>%
  dplyr::filter(
    var == "VALUE" |
    var  %in% c("VALUE1", "VALUE2")
  )


# 2.3. Eligibility criteria --------------------------------------------------

# NOTE: Modify labels as required for project

# 2.3.1 INCLUSION 1: [Minimim pre-index enrollment period] -------------------

# 2.3.2 INCLUSION 2: [Minimim post-index follow-up]        -------------------

# Usually requires >=1 day follow-up
# If possible, avoid using other follow-up restrictions as this
# can induce immortality bias

# 2.3.3 INCLUSION 3: [Disease-related diagnosis criteria]  -------------------

# 2.3.4 INCLUSION 4: [DIsease-modifying criteria, ie severity] ---------------


# 2.3.5  EXCLUSION 1: [No cancer malignnacy within 5 years prior to index] -------------------

# 2.3.6  EXCLUSION 2: [Not pregnant or nursing at index or within 30 days prior to index] -------------------

# 2.3.7  EXCLUSION 3: [Describe] -------------------

# 2.4. Index Date --------------------------------------------------

# Index date is the date that a patient first becomes eligible for
# inclusion in the study cohort, that is they satisfy all eligibility criteria

# Note that for some designs,
# patients can become eligible at multiple points in time, in which case
# they may have more than one index date. In such cases, multiple index dates can
# be extracted and used using a mixed effects model or a random index date
# may be selected.

# d2 <- raw %>%
#   dplyr::mutate(
      # Derive composite indicating patient meets all eligibility criteria
#     eligible_all = [code block]
      # Derive index date
      index_date = [code block]
#   )


# 2.5. Subgroups  --------------------------------------------------

# 2.4.1 SUBGROUP [Describe]

# 2.4.2 [OPTIONAL FOR MORE SUBGROUPS]


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
#  [ ] Pre-index date of enrollment (pre_enroll_date)
#  [ ] Post-index end of enrollment date (used for censoring) (post_disenroll_date)
#  [ ] Inclusion/exclusion flags
#  [ ] Subgroup flags


# dat <-
# # Left join overall population with eligibility criteria and index dates
# dplyr::left_join(
#   d1, d2
# ) %>%
# # Left join subpopulation flags
# dplyr::left_join(
#   purrr::reduce(
#     list(subgroup1, subgroup2),
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

# How many patients satisfy the first inclusion criteria
# dat %>%
#   dplyr::filter(inclusion_flag == "YES") %>% # Or: inclusion_flag == [1 | "Y" | "y"]
#   dplyr::pull(usubjid) %>%
#   unique %>% length

# How many missing cells are there for the first inclusion criteria?
# dat %>%
#   dplyr::filter(is.na(inclusion_flag) | inclusion_flag=="") %>%
#   dplyr::pull(usubjid) %>%
#   unique %>% length

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
'
  return(line1)
}
