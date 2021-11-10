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

template_rfunction <- function(target_file = "template.R"){

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

# 1.2 Custom functions -------------------------------------------------------
rfuns <- list.files("R/")
lapply(rfuns, function(x) source(paste0("R/",x))) # Loads functions to memory


# 1.3. Load data dependencies ------------------------------------------------

# read.csv()
# load()
# haven::read_sas()
# haven::read_xpt()


'

}


