library(haven)
library(tidyverse)

globorisk_path <- "../hic-lipid-treatment/1_code/globorisk/"


# Coefficients ------------------------------------------------------------

# read in coefficients
coef_lab <- read_stata(paste0(globorisk_path, "coeff_lab.dta"))
coef_fatal <- read_stata(paste0(globorisk_path, "coeff_fatal.dta"))
coef_office <- read_stata(paste0(globorisk_path, "coeff_office.dta"))

# drop Stata rowname variable
coef_lab[["_rowname"]] <- NULL
coef_fatal[["_rowname"]] <- NULL
coef_office[["_rowname"]] <- NULL

# bind into one dataset
coefs <- bind_rows(coef_lab, coef_fatal, coef_office, .id = "type")

coefs$type <- case_when(
  coefs$type == 1 ~ "lab",
  coefs$type == 2 ~ "fatal",
  coefs$type == 3 ~ "office"
)


# CVD rates ---------------------------------------------------------------

# get fatal, non-fatal CVD rate files
fnfcvdr_path <- paste0(globorisk_path, "CVDrates_p")
fnfcvdr_files <- dir(fnfcvdr_path, pattern = ".dta", full.names = TRUE)
fnfcvdr_filenames <- dir(fnfcvdr_path, pattern = ".dta")
names(fnfcvdr_files) <- fnfcvdr_filenames

# get fatal CVD rate files
fcvdr_path <- paste0(globorisk_path, "Fatal_CVDrates_p")
fcvdr_files <- dir(fcvdr_path, pattern = ".dta", full.names = TRUE)
fcvdr_filenames <- dir(fcvdr_path, pattern = ".dta")
names(fcvdr_files) <- fcvdr_filenames

cvdr_files <- c(fnfcvdr_files, fcvdr_files)

# read in all files
cvdr <-
  cvdr_files %>%
  map_dfr(read_stata, .id = "file")

# drop Stata rowname variable
cvdr[["_rowname"]] <- NULL

# clean up
cvdr <-
  cvdr %>%
  mutate(
    type = case_when(
      str_detect(file, "fnfchdstr") ~ "FNF",
      str_detect(file, "fatalchdstr") ~ "F"
    ),
    file = str_remove(file, "^(fnfchdstr)|(fatalchdstr)"),
    iso = str_extract(file, "^([A-Z]{3})"),
    sex = as.numeric(str_extract(file, "[01]")),
    cvd = if_else(!is.na(fnfchdstr_f), fnfchdstr_f, fatalchdstr),
  ) %>%
  group_by(iso, type, sex, agec) %>%
  arrange(year) %>%
  mutate(
    cvd_0 = cvd,
    cvd_1 = lead(cvd, 1),
    cvd_2 = lead(cvd, 2),
    cvd_3 = lead(cvd, 3),
    cvd_4 = lead(cvd, 4),
    cvd_5 = lead(cvd, 5),
    cvd_6 = lead(cvd, 6),
    cvd_7 = lead(cvd, 7),
    cvd_8 = lead(cvd, 8),
    cvd_9 = lead(cvd, 9)
  ) %>%
  ungroup() %>%
  group_by(iso, type, sex, year) %>%
  arrange(agec) %>%
  mutate(
    across(cvd_0:cvd_9, lead, n = 1, .names = "lead1_{.col}"),
    across(cvd_0:cvd_9, lead, n = 2, .names = "lead2_{.col}")
  ) %>%
  ungroup() %>%
  arrange(iso, type, sex, agec, year) %>%
  filter(2000 <= year & year <= 2020) %>%
  select(
    iso,
    year,
    type,
    sex,
    agec,
    cvd_0:cvd_9,
    lead1_cvd_0:lead1_cvd_9,
    lead2_cvd_0:lead2_cvd_9
  )


# Risk factor levels ------------------------------------------------------

# get fatal, non-fatal CVD rate files
rf_path <- paste0(globorisk_path, "Risk factor levels by iso")
rf_files <- dir(rf_path, pattern = ".dta", full.names = TRUE)
rf_filenames <- dir(rf_path, pattern = ".dta")
names(rf_files) <- rf_filenames

# read in all files
rf <-
  rf_files %>%
  map_dfr(read_stata, .id = "file")

# drop Stata rowname variable
rf[["_rowname"]] <- NULL

# clean up
rf <-
  rf %>%
  mutate(
    file = str_remove(file, "^rf"),
    iso = str_extract(file, "^([A-Z]{3})")
  ) %>%
  select(-file) %>%
  pivot_longer(
    cols = c(
      "sbpm",
      "tcm",
      "dmm",
      "smkm",
      "sbpf",
      "tcf",
      "dmf",
      "smkf",
      "bmim",
      "bmif"
      )
  ) %>%
  mutate(
    sex = if_else(str_detect(name, ".*f$"), 1, 0),
    name = paste0("mean_", str_sub(name, end = -2)),
  ) %>%
  pivot_wider(names_from = "name", values_from = "value") %>%
  mutate(mean_bmi = mean_bmi / 5) %>%
  relocate(iso)

usethis::use_data(coefs, cvdr, rf, internal = TRUE, overwrite = TRUE)
