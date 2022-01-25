library(haven)
library(tidyverse)

globorisk_path <- "data-raw/_data/"


# Coefficients ------------------------------------------------------------

# read in coefficients
coef_lab <- read_stata(paste0(globorisk_path, "coeff_lab.dta"))
coef_fatal <- read_stata(paste0(globorisk_path, "coeff_fatal.dta"))
coef_office <- read_stata(paste0(globorisk_path, "coeff_office.dta"))
coef_lac_lab <- read_csv(paste0(globorisk_path, "coeff_lac_lab.csv"))
coef_lac_office <- read_csv(paste0(globorisk_path, "coeff_lac_office.csv"))

# drop Stata rowname variable
coef_lab[["_rowname"]] <- NULL
coef_fatal[["_rowname"]] <- NULL
coef_office[["_rowname"]] <- NULL

# bind into one dataset
coefs <-
  bind_rows(coef_lab,
            coef_fatal,
            coef_office,
            coef_lac_lab,
            coef_lac_office,
            .id = "type")

coefs$lac <- case_when(
  coefs$type == 1 ~ 0,
  coefs$type == 2 ~ 0,
  coefs$type == 3 ~ 0,
  coefs$type == 4 ~ 1,
  coefs$type == 5 ~ 1
)

coefs$type <- case_when(
  coefs$type == 1 ~ "lab",
  coefs$type == 2 ~ "fatal",
  coefs$type == 3 ~ "office",
  coefs$type == 4 ~ "lab",
  coefs$type == 5 ~ "office"
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
  select(iso, year, type, sex, agec, cvd)

cvdr_grd <-
  expand_grid(
    iso = unique(cvdr$iso),
    sex = unique(cvdr$sex),
    type = unique(cvdr$type),
    age = seq(40, 85),
    base_year = 2000:2020,
    year = 2000:2029
  ) %>%
  mutate(
    agec = as.integer(ifelse(age < 85, trunc(age / 5) - 7, 10)),
    agec_j = ifelse(
      trunc((age + year - base_year) / 5) - 7 <= agec,
      agec,
      trunc((age + year - base_year) / 5) - 7
    )
  )

cvdr <-
  left_join(
    cvdr_grd,
    cvdr,
    by = c("iso", "sex", "type", "year", "agec_j" = "agec")
  ) %>%
  mutate(
    i = year - base_year,
    year = base_year
  ) %>%
  select(-agec_j, -base_year) %>%
  filter(i >= 0) %>%
  pivot_wider(
    names_from = i,
    names_prefix = "cvd_",
    values_from = cvd
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
