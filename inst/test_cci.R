#' Derive Charlson comorbidity index (CCI)
#'
#' A dataset of CCI comorbidities and indices
#'
#'
#' @param data Data frame containing ICD codes; date of index must be 'dt'; date of ICD diagnosis code must be 'dt2'; unique patient id must be 'person_id'
#' @param cohort_ids Vector of unique cohort patient IDs. If a patient ID is not captured in the `data` argument, it is assumed that the corresponding patient had no comorbidities during the specified lookback period. If NULL, then `cohort_ids <- unique(data$person_id)`.
#' @param lookback_days numeric; number of days prior to index date over which the CCI is to be aggregated.
#' @param cci_codes string; column name containing diagnosis codes for CCI (only ICD9CM and ICD10CM are used).
#' @param icd_codes string; column name containing diagnosis codes for ICD datasets.
#' @param cci_labels list; list of labels for CCI components and derived indices.
#' @param clear_files logical; indicates whether to delete any ICD codes downloaded during execution.
#'
#'
#' @export
#'
#' @description
#'
#'
#' This function returns a data set of Charlson comorbidity indices as well as corresponding comorbidity dummy variables.
#'
#' @examples
#'
#' library(dplyr)
#'
#' # Make fake data
#' test <- data.frame(
#'     person_id = c(1,1,2),
#'     condition_source_value = c('C801', 'I501', '1629'),
#'     dt = c('2020-01-01', '2020-01-01', '2020-01-01'),
#'     dt2 = c('2019-09-30', '2019-04-01', '2019-06-30')
#' )
#'
#' # Extract unique cohort ids
#' cohort_ids <- test$person %>% unique
#'
#' result <- rwetools::get_charlson_index(data=test, cohort_ids=test$person_id %>% unique)


get_charlson_index <- function(
  data,
  cohort_ids = NULL,
  lookback_days=365,
  cci_codes = 'condition_source_value',
  icd_codes = 'code',
  cci_labels = NULL,
  clear_files = TRUE
){
  # Assign cohort_ids
  if(is.null(cohort_ids)) cohort_ids <- unique(data$person_id)

  # Assign CCI labels
  if (is.null(cci_labels)){
    cci_labels <- list(
      ami="CCI: Myocardial Infarction",
      chf="CCI: Congestive heart failure",
      pvd="CCI: Periphral Vascular Disease",
      cevd="CCI: Cerebrovascular Disease",
      dementia="CCI: Dementia",
      copd="CCI: Chronic pulmonary obstructive disease",
      rheumd="CCI: Rheumatoid disease",
      pud="CCI: Peptic ulcer disease",
      mld="CCI: Mild liver disease",
      diab= "CCI: Diabetes without complications",
      diabwc="CCI: Diabetes with complications",
      hp="CCI: Paraplegia and hemiplegia",
      rend="CCI: Renal disease",
      canc="CCI: Cancer",
      msld="CCI: Moderate or severe liver disease",
      metacanc="CCI: Metastatic carcinoma",
      aids="CCI: HIV/AIDS",
      cci_score = "Charlson Comorbidity Index, score",
      cci_index = "Charlson Comorbidity Index, categorical"
    )
  } else {

  }

  # Data prep
  #data <- data %>% tibble::as_tibble()
  fn_dt <- function(x){
    if(is.factor(x) | is.character(x)) x <- lubridate::ymd(x)

    x
  }

  data <- data %>%
    dplyr::mutate(
      dplyr::across(c(dt, dt2), fn_dt)
    )


  # Load universe of ICD codes
  #icd::set_icd_data_dir("./icd")

  icd9 <- icd9_2014 %>% #icd_codes$icd9_2014 %>%
    dplyr::select(code=icd_codes) %>% dplyr::mutate(version="ICD9CM", code = code %>% as.character)
  icd10 <-icd10_2019 %>%
    dplyr::select(code=icd_codes) %>% dplyr::mutate(version="ICD10CM", code = code %>% as.character)

  #if(clear_files==TRUE) unlink("./icd", recursive = TRUE)

  # CCI data: temporary ID
  data <- cbind(
    data,
    data %>%
      dplyr::select(person_id, dt) %>%
      dplyr::mutate(tmpID1 = paste0(person_id, dt)) %>%
      dplyr::select(tmpID1),
    data %>% dplyr::select(person_id, dt) %>%
      dplyr::group_by(person_id, dt) %>%
      dplyr::mutate(obsno=dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        tmpID = paste0(person_id, dt, obsno)
      ) %>%
      dplyr::select(tmpID)
  ) %>%
    dplyr::rename(code=cci_codes)

  # Merge to codes to observations
  cci_icd10 <- dplyr::left_join(
    data, icd10
  ) %>%
    dplyr::filter(!is.na(version))

  cci_icd9 <- dplyr::left_join(
    data, icd9
  ) %>%
    dplyr::filter(!is.na(version))


  # Apply CCI
  cci_icd10 <- comorbidity::comorbidity(
    x=cci_icd10, id = 'tmpID', score = 'charlson', code = 'code', assign0 = FALSE, icd = 'icd10'
  )
  cci_icd9 <- comorbidity::comorbidity(
    x=cci_icd9, id = 'tmpID', score = 'charlson', code = 'code', assign0 = FALSE, icd = 'icd9'
  )

  data2 <- dplyr::left_join(
    data,
    rbind(cci_icd9, cci_icd10)
  )

  # Derive components of CCI
  # Assume any CCI component during lookback period qualifies as having comorbidity

  tmpFun1 <- function(x){
    x <- ifelse(is.na(x), 0, x)
  }

  tmpFun2 <- function(x){
    x <- ifelse(sum(x==1)>0, 1, 0)
  }

  # If no condition occurs outside of look back period, then set condition to zero
  data3 <- data2 %>%
    dplyr::select(-score, -index, -wscore, -windex) %>%
    dplyr::group_by(tmpID) %>%
    dplyr::mutate(obsno = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = c(ami:aids), names_to = "com", values_to = "y") %>%
    dplyr::mutate(
      y = ifelse(
        as.numeric(dt-dt2)>=0 & as.numeric(dt-dt2)<=365, y, 0
      )
    ) %>%
    tidyr::pivot_wider(id_cols = c(person_id:tmpID, obsno), names_from = com, values_from = y) %>%
    dplyr::mutate(dplyr::across(c(ami, chf, pvd, cevd, dementia, copd, rheumd, pud, mld, diab, diabwc, hp, rend, canc, msld, metacanc, aids), tmpFun1)) %>%
    dplyr::group_by(tmpID1) %>%
    dplyr::mutate(dplyr::across(c(ami, chf, pvd, cevd, dementia, copd, rheumd, pud, mld, diab, diabwc, hp, rend, canc, msld, metacanc, aids), tmpFun2)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-obsno)
  # Calculate CCI scores
  data4 <- data3 %>%
    dplyr::mutate(
      wt_aids = ifelse(aids==1, 6, 0),
      wt_metacanc = ifelse(metacanc==1, 6, 0),
      wt_rend = ifelse(rend==1, 2, 0),
      wt_diabwc = ifelse(diabwc==1, 2, 0),
      wt_diab = ifelse(diab==1 & diabwc==0, 1, 0),
      wt_hp = ifelse( hp==1, 2, 0),
      wt_cancer = ifelse(canc==1 & metacanc==0, 2, 0),
      wt_msld = ifelse(msld==1, 3, 0),
      wt_ami = ifelse(ami==1, 1, 0),
      wt_chf = ifelse(chf==1, 1, 0),
      wt_pvd = ifelse(pvd==1, 1, 0),
      wt_cevd = ifelse(cevd==1, 1, 0),
      wt_dementia  = ifelse(dementia ==1, 1, 0),
      wt_copd = ifelse(copd==1, 1, 0),
      wt_rheumd = ifelse(rheumd==1, 1, 0),
      wt_pud  = ifelse(pud ==1, 1, 0),
      wt_mld  = ifelse(mld ==1 & msld ==0, 1, 0)
    ) %>%
    dplyr::select(tmpID1, dplyr::contains('wt_')) %>%
    tidyr::pivot_longer(
      cols = c(wt_aids:wt_mld), names_to = 'comp', values_to = 'wt'
    ) %>%
    dplyr::group_by(tmpID1) %>%
    dplyr::mutate(cci_score = sum(wt)) %>%
    dplyr::slice(1) %>% dplyr::ungroup() %>%
    dplyr::select(tmpID1, cci_score)

  data5 <- dplyr::left_join(data3, data4) %>%
    dplyr::mutate(
      cci_index = dplyr::case_when(
        cci_score==0 ~ '0',
        cci_score==1 ~ '1',
        cci_score==2 ~ '2',
        cci_score>2 ~ '3+'
      )
    )

  # If patient is not in derived CCI data frame, we assume they have no comorbidities
  data6 <- dplyr::left_join(
    data.frame(person_id=cohort_ids),
    data5
  ) %>%
    dplyr::mutate(
      cci_index = ifelse(is.na(cci_index), "0", cci_index),
      cci_score = ifelse(is.na(cci_score), 0, cci_score)
    ) %>%
    dplyr::mutate(dplyr::across(c(ami, chf, pvd, cevd, dementia, copd, rheumd, pud, mld, diab, diabwc, hp, rend, canc, msld, metacanc, aids), tmpFun1))

  keep <- c(
    "person_id", "dt", "ami", "chf", "pvd",
    "cevd", "dementia", "copd", "rheumd",
    "pud", "mld", "diab", "diabwc", "hp",
    "rend", "canc", "msld", "metacanc", "aids",
    "cci_score", "cci_index"
  )

  data7 <- data6 %>%
    dplyr::select(!!!keep)

  labelled::var_label(data7 ) <- cci_labels

  list(
    data=data7,
    cci_labels = cci_labels
  )


}


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
