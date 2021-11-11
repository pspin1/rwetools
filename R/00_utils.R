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




#' Create R function using template
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

template_data_clean <- function(dir = "inst/1_clean", replace = TRUE){

  # Generate boilerplate text for scripts




  # ... Issue errors if directory already exists
  if (replace == TRUE) {
    warning("If directory exists, then it will be overwritten.")

  } else {
    if (dir.exists(dir)) stop("Directory already exists")

  }

  # ... Issue

  dir.create(dir, recursive = TRUE)



  # 1. Study population -------------------------------------------------------
  # Code for defining study population, eligibility criteria,
  # subgroups, index dates, and data set enrollment and dis-enrollment fields
  # File name
  target_file <- "01_study_pop.R"

  # Connect to file
  file_conn <- base::file(paste0(dir, "/", target_file))

  # Code
  lines <- script_clean_index()

  # Write lines to file connection

  writeLines(text = lines, con = file_conn)

  # Close file connection
  close(file_conn)

  # Open user-facing file
  file.edit(paste0(dir, "/", target_file))


  # 2. Demographic variables  -------------------------------------------------
  # Code for defining demographic variables
  # Example: Age, sex, geography

  target_file <- "02_demo.R"

  # Connect to file
  file_conn <- base::file(paste0(dir, "/", target_file))

  # Code
  lines <- script_clean_demo()

  # Write lines to file connection

  writeLines(text = lines, con = file_conn)

  # Close file connection
  close(file_conn)

  # Open user-facing file
  file.edit(paste0(dir, "/", target_file))


}





