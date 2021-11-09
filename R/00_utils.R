#' Create project structure
#'
#'
#' @param dir character denoting path of directory where the project will be created
#'
#' @export
#'
create_project <- function(dir) {

  # Errors   ------------------------------------------

  #... Issue errors if directory already exists
  if(dir.exists(dir)) stop("Directory already exists")

  # Warnings  ------------------------------------------



  # Create initial directory ---------------------------

  #... Create directory
  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)


  # Create subdirectories ------------------------------
  subdir <- list(
    fun = c("R"),
    data = c("data", "1_raw", "2_intermediate", "3_analysis", "9_ancillary"),
    scripts = c("inst", "1_clean", "2_describe", "3_model", "4_validate", "5_report", "9_adhoc"),
    docs = c("docs"),
    rmd = c("rmd", "1_draft_reports", "2_final_reports", "9_adhoc_reports")
  )



}

