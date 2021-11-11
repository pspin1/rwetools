#' ICD9CM medical codes (2014 edition)
#'
#' A dataset of ICD9CM codes for 2014. Courtesy of the `icd` package.
#'
#'
#' @format A data frame
#' \describe{
#'   \item{code}{ICD9CM code.}
#'   \item{leaf}{Logical indicating whether code is billable}
#'   \item{short_desc}{Short literal description of code}

#'   ...
#' }

"icd9_2014"


#' ICD10CM medical codes (2019 edition)
#'
#' A dataset of ICD10CM codes for 2019. Courtesy of the `icd` package.
#'
#'
#' @format A data frame
#' \describe{
#'   \item{code}{ICD10CM code.}
#'   \item{leaf}{Logical indicating whether code is billable}
#'   \item{short_desc}{Short literal description of code}

#'   ...
#' }

"icd10_2019"


#' General equivalence mapping from ICD10CM to ICD9CM (2018 edition)
#'
#' A dataset mapping ICD10CM codes to ICD9CM codes (2018 edition)
#'
#'
#' @format A data frame
#' \describe{
#'   \item{icd10}{ICD10CM code.}
#'   \item{icd9}{ICD9CM code.}

#'   ...
#' }

"gem_10_to_9"


#' General equivalence mapping from ICD9CM to ICD10CM (2018 edition)
#'
#' A dataset mapping ICD9CM codes to ICD10CM codes (2018 edition)
#'
#'
#' @format A data frame
#' \describe{
#'   \item{icd9}{ICD9CM code.}
#'   \item{icd10}{ICD10CM code.}

#'   ...
#' }

"gem_9_to_10"


#' Simulated patient dataset with observed ICD10CM codes
#'#'
#'
#' @format A data frame
#' \describe{
#'   \item{id}{Unique patient identifier.}
#'   \item{code}{ICD10CM code.}

#'   ...
#' }

"sim_icd10_pat"


