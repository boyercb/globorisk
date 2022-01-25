# globorisk <img src="man/figures/logo.png" align="right" width="180" height="180"/>

<!-- badges: start -->
<!-- badges: end -->
The `globorisk` package calculates risk estimates for cardiovascular disease
in R using Globorisk.

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
# not approved yet
# install.packages("globorisk")
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

# References
Ueda, Peter, Mark Woodward, Yuan Lu, Kaveh Hajifathalian, Rihab Al-Wotayan,
Carlos A. Aguilar-Salinas, Alireza Ahmadvand, et al. "Laboratory-Based and
Office-Based Risk Scores and Charts to Predict 10-Year Risk of Cardiovascular
Disease in 182 Countries: A Pooled Analysis of Prospective Cohorts and Health
Surveys." The Lancet Diabetes & Endocrinology 5, no. 3 (March 1, 2017):
196–213. https://doi.org/10.1016/S2213-8587(17)30015-3.

Hajifathalian, Kaveh, Peter Ueda, Yuan Lu, Mark Woodward, Alireza Ahmadvand,
Carlos A Aguilar-Salinas, Fereidoun Azizi, et al. “A Novel Risk Score to
Predict Cardiovascular Disease Risk in National Populations (Globorisk): A
Pooled Analysis of Prospective Cohorts and Health Examination Surveys.” The
Lancet Diabetes & Endocrinology 3, no. 5 (May 1, 2015): 339–55.
https://doi.org/10.1016/S2213-8587(15)00081-9.

