#' Globorisk risk calculator
#'
#' Computes the 10-year risk of CVD using globorisk, a prediction model for the
#' risk of cardiovascular disease in 182 countries.
#'
#' @param sex biological sex (0 = man, 1 = woman)
#' @param age patient age (years)
#' @param sbp systolic blood pressure (mmHg)
#' @param tc total cholesterol (mmol/L)
#' @param dm diabetes mellitus (0 = no, 1 = yes)
#' @param smk current smoker (0 = no, 1 = yes)
#' @param bmi body mass index (kg/m^2)
#' @param iso ISO code for country of interest
#' @param year baseline year
#' @param time follow up time (default is 10-years)
#' @param version calculator version, options are 'lab', 'office', or 'fatal'
#' @param type output type, options are 'risk', 'survival', or 'all'
#'
#' @return If type = 'risk' output is a vector with estimated 10-year risk for
#'   CVD, if type = 'survival' output is a vector with estimated 10-year
#'   survival, if type = 'all' output is a data.frame with intermediate
#'   calculations
#'
#' @export
#'
#' @examples
#' library(globorisk)
#'
#' globorisk(
#'   sex = c(1, 0, 0),
#'   age = c(52, 60, 65),
#'   sbp = c(140, 160, 170),
#'   tc = c(4.5, 5, 5),
#'   dm = c(1, 1, 1),
#'   smk = c(0, 1, 1),
#'   iso = c("AFG", "AFG", "USA"),
#'   year = c(2000, 2000, 2020),
#'   version = "lab",
#'   type = "risk"
#' )
#' @references
#' Ueda, Peter, Mark Woodward, Yuan Lu, Kaveh Hajifathalian, Rihab Al-Wotayan,
#' Carlos A. Aguilar-Salinas, Alireza Ahmadvand, et al. "Laboratory-Based and
#' Office-Based Risk Scores and Charts to Predict 10-Year Risk of Cardiovascular
#' Disease in 182 Countries: A Pooled Analysis of Prospective Cohorts and Health
#' Surveys." The Lancet Diabetes & Endocrinology 5, no. 3 (March 1, 2017):
#' 196–213. https://doi.org/10.1016/S2213-8587(17)30015-3.
#'
#' Hajifathalian, Kaveh, Peter Ueda, Yuan Lu, Mark Woodward, Alireza Ahmadvand,
#' Carlos A Aguilar-Salinas, Fereidoun Azizi, et al. “A Novel Risk Score to
#' Predict Cardiovascular Disease Risk in National Populations (Globorisk): A
#' Pooled Analysis of Prospective Cohorts and Health Examination Surveys.” The
#' Lancet Diabetes & Endocrinology 3, no. 5 (May 1, 2015): 339–55.
#' https://doi.org/10.1016/S2213-8587(15)00081-9.
#'
globorisk <- function(
  sex,
  age,
  sbp,
  tc = NA,
  dm = NA,
  smk,
  bmi = NA,
  iso,
  year,
  time = 10,
  version = c('lab', 'office', 'fatal'),
  type = 'risk'
) {

  # check arguments
  if (time < 1 | time > 10) {
    stop("Invalid time argument, must be between 1 and 10")
  }

  if (any(!sex %in% c(0, 1) & !is.na(sex))) {
    stop("Invalid values in sex variable, must be 0 = man and 1 = woman!")
  }

  if (any(age < 40 & !is.na(age))) {
    stop("Invalid values in age variable, must be greater than 40!")
  }

  if (any(!dm %in% c(0, 1) & !is.na(dm))) {
    stop("Invalid values in dm variable, must be 0 = no and 1 = yes!")
  }

  if (any(!smk %in% c(0, 1) & !is.na(smk))) {
    stop("Invalid values in smk variable, must be 0 = no and 1 = yes!")
  }

  if (any((tc < 1.75 | tc > 20) & !is.na(tc))) {
    warning("Implausible values in tc variable, are you sure you entered in mmol/L?")
  }

  if (any((sbp < 70 | sbp > 270) & !is.na(sbp))) {
    warning("Implausible values in sbp variable, are you sure you entered in mmHg?")
  }

  if (any((bmi < 10 | bmi > 80) & !is.na(bmi))) {
    warning("Implausible values in bmi variable, are you sure you entered in kg/m^2?")
  }

  if (any(year < 2000 | year > 2020)) {
    stop("Invalid baseline year variable, must be between 2000 and 2020!")
  }

  if (any(!iso %in% unique(cvdr$iso))) {
    stop("ISO not found in globorisk database!")
  }

  if (!type %in% c('risk', 'survival', 'all')) {
    stop("Invalid type argument, must be 'risk', 'survival', or 'all'")
  }

  if (version == 'lab') {
    coefs <- subset(coefs, type == "lab")
    cvdr <- subset(cvdr, type == "FNF")
  } else if (version == 'office') {
    coefs <- subset(coefs, type == "office")
    cvdr <- subset(cvdr, type == "FNF")
  } else if (version == 'fatal') {
    coefs <- subset(coefs, type == "lab")
    cvdr <- subset(cvdr, type == "F")
  } else {
    stop("Invalid version argument, must be 'lab', 'office', or 'fatal'!")
  }

  # create data frame
  d <- data.frame(
    iso = toupper(iso),
    sex = as.integer(sex),
    year = as.integer(year),
    age = as.integer(trunc(age)),
    agec = as.integer(trunc(age / 5) - 7),
    sbp = sbp / 10,
    tc = tc,
    dm = as.integer(dm),
    smk = as.integer(smk),
    bmi = bmi / 5,
    stringsAsFactors = FALSE
  )

  # use time minus one
  time <- time - 1

  # merge baseline cvd rates
  d <-
    merge(
      d,
      cvdr,
      by = c("iso", "year", "sex", "agec"),
      all.x = TRUE,
      sort = FALSE
    )

  # merge mean risk factor levels
  d <-
    merge(
      d,
      rf,
      by = c("iso", "sex", "agec"),
      all.x = TRUE,
      sort = FALSE
    )

  # center values using population mean risk factor levels
  for (var in c("sbp", "tc", "dm", "smk", "bmi")) {
    d[[paste0(var, "_c")]] <- d[[var]] - d[[paste0("mean_", var)]]
  }

  for (t in 0:time) {

    # calculate the time-varying hazard ratio for each individual
    if (version == "lab" | version == "fatal") {
      # version with laboratory measures
      d[[paste0('hrC_', t)]] <- exp(
        d$sbp_c * coefs[["main_sbpc"]] +
          d$tc_c * coefs[["main_tcc"]] +
          d$dm_c * coefs[["main__Idm_1"]] +
          d$smk_c * coefs[["main_smok"]] +
          d$sex * d$dm_c * coefs[["main_sexdm"]] +
          d$sex * d$smk_c * coefs[["main_sexsmok"]] +
          (d$age + t) * d$sbp_c * coefs[["tvc_sbpc"]] +
          (d$age + t) * d$tc_c * coefs[["tvc_tcc"]] +
          (d$age + t) * d$dm_c * coefs[["tvc_dm"]] +
          (d$age + t) * d$smk_c * coefs[["tvc_smok"]]
      )
    } else {
      # version with only office measures
      d[[paste0('hrC_', t)]] <- exp(
        d$sbp_c * coefs[["main_sbpc"]] +
          d$bmi_c * coefs[["main_bmi5c"]] +
          d$smk_c * coefs[["main_smokc"]] +
          d$sex * d$smk_c * coefs[["main_sexsmokc"]] +
          (d$age + t) * d$sbp_c * coefs[["tvc_sbpc"]] +
          (d$age + t) * d$smk_c * coefs[["tvc_smokc"]] +
          (d$age + t) * d$bmi_c * coefs[["tvc_bmi5c"]]
        )

    }

    # calculate the hazard rate by multiplying by base rate
    d[[paste0('hzcvd_', t)]] <-
      d[[paste0('hrC_', t)]] * d[[paste0("cvd_", t)]]

    # calculate survival at time t
    d[[paste0('surv_', t)]] <- exp(-(d[[paste0('hzcvd_', t)]]))

  }

  # calculate total survival
  d$totsurv <- apply(d[, paste0('surv_', 0:time), drop = F], 1, prod)

  # calculate cumulative risk
  d$globorisk <- 1 - d$totsurv

  ret <- switch(
    type,
    risk = d$globorisk,
    survival = d$survival,
    all = d
  )

  return(ret)

}
