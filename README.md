# globorisk <img src="man/figures/logo.png" align="right" width="180" height="180"/>

<!-- badges: start -->
<!-- badges: end -->

Globorisk is a cardiovascular disease risk score that predicts risk of
heart attack or stroke in healthy individuals (those who have not yet had a
heart attack or stroke) in 182 countries. It uses information on a
person’s country of residence, age, sex, smoking, diabetes, blood pressure and
cholesterol to predict the chance that they would have a heart attack or stroke
in the next 10 years. If the person does not have a recent diabetes or
cholesterol test, they can use the office-based version of Globorisk which is
based on body weight and height instead.

## Installation

You can install the released version of globorisk from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("globorisk")
```

You can also install the latest development version from the Github repository.

``` r
# install.package("devtools")
devtools::install_github("boyercb/globorisk")
```

## Usage

``` r
library(globorisk)

# estimate 10-year risk of CVD using laboratory calculator 
globorisk(
  sex = 1,
  age = 52,
  sbp = 140,
  tc = 4.5,
  dm = 1,
  smk = 0,
  iso = "ZAF",
  year = 2020,
  version = "lab",
  type = "risk"
)

# estimate 10-year survival using laboratory calculator 
globorisk(
  sex = 1,
  age = 52,
  sbp = 140,
  tc = 4.5,
  dm = 1,
  smk = 0,
  iso = "ZAF",
  year = 2020,
  version = "lab",
  type = "survival"
)

# estimate 10-year risk of CVD using office calculator 
globorisk(
  sex = 1,
  age = 52,
  sbp = 140,
  smk = 0,
  bmi = 28,
  iso = "ZAF",
  year = 2020,
  version = "office",
  type = "risk"
)

# also works on vectors
globorisk(
  sex = c(1, 0, 0),
  age = c(52, 60, 65),
  sbp = c(140, 160, 170),
  smk = c(0, 1, 1),
  bmi = c(20, 30, 40),
  iso = c("AFG", "AFG", "USA"),
  year = c(2000, 2000, 2020),
  version = "office",
  type = "risk"
)



```

