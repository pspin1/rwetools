#' Simulate a fake data set of ICD10 codes
#'
#'
#' @param n1 Number of patients to simulate
#' @param n2 Number of unique diagnoses to select
#'
#' @export

sim_codes <- function(
  n1 = 1000,
  n2 = 1000
) {

  data("icd10cm_2019", envir=environment())
  d <- icd10_2019

  # Extract unique billable codes
  d <- d %>%
    dplyr::filter(billable == TRUE) %>%
    dplyr::mutate(
      code = as.character(code)
    )

  codes <- unique(d$code)

  codes_sample <- base::sample(
    codes,
    size = n2,
    replace = FALSE,
    prob = NULL
  )

  # Probabilities of having diagnosis
  # Range 1% to 15%

  codes_prob <- stats::runif(
    1000,
    min = 0.01,
    max = 0.10
  )

  codes_df <- data.frame(
    code = codes_sample,
    p = codes_prob
  )

  # Combine codes and probabilities into a single data set


  # Create a data set containing one potential diagnosis code for each
  # patient. For n1 patients and n2 diagnosis codes, there should be
  # n1 x n2 records


  pt_codes <- expand.grid(
    id = 1:n1,
    code = codes_sample
  )

  # Simulate realization of diagnosis code using Bernoulli distribution
  pt_codes <- pt_codes %>%
    dplyr::left_join(
      codes_df
    )

  pt_codes$y <- stats::rbinom(
    n=nrow(pt_codes),
    size = 1,
    prob = pt_codes$p
  )

  # Extract observed diagnoses only
  # Add patients with no observed diagnoses
  # into the data set after but assign NA for all
  # diagnosis-related columns

  pt_codes_yes <- pt_codes %>%
    dplyr::filter(
      y==1
    ) %>%
    dplyr::select(-p, -y) %>%
    dplyr::left_join(d)

  pt_codes_no <- pt_codes %>%
    dplyr::filter(
      !id %in% pt_codes_yes$id
    ) %>%
    dplyr::select(id)

  pt_codes <-
    dplyr::bind_rows(
      list(pt_codes_yes, pt_codes_no)
    )

  ### Add days prior to index that
  ### diagnosis codes were observed
  ### Assumes any number of days from
  ### 1:1500 are equally plausible
  pt_codes$t_vs_index = round(ifelse(
    is.na(pt_codes$code),
    NA,
    stats::runif(nrow(pt_codes), -1500, -1)
  ))


  return(pt_codes)


}

