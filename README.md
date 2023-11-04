
<!-- README.md is generated from README.Rmd. Please edit that file -->

# co2fluxtent <a href='https://github.com/PaulESantos/co2fluxtent'><img src='man/figures/tent.jfif' align="right" height="150" width="350" /></a>

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/r-lib/lifecycle/branch/master/graph/badge.svg)](https://app.codecov.io/gh/r-lib/lifecycle?branch=master)  
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![R-CMD-check](https://github.com/PaulESantos/co2fluxtent/workflows/R-CMD-check/badge.svg)](https://github.com/PaulESantos/co2fluxtent/actions)
[![R-CMD-check](https://github.com/PaulESantos/co2fluxtent/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PaulESantos/co2fluxtent/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Welcome to the official repository for the `co2fluxtent` package – an R
tool designed to facilitate the extraction of Net Ecosystem Exchange,
Transpiration, and Evapotranspiration Flux Measurements from LiCOR 7500
data.

### About `co2fluxtent`

`co2fluxtent` is a comprehensive R package that simplifies the process
of analyzing data from LiCOR 7500 gas analyzers. This package provides a
complete suite of tools for theoretical modeling, curve-fitting, and
data analysis. It’s ideal for researchers, ecologists, and environmental
scientists looking to unlock insights into the exchange of CO2 and H2O
in ecosystem flux data.

### Introduction

This repository serves as your gateway to understanding and utilizing
`co2fluxtent`. Our goal is to equip you with the knowledge and resources
needed to perform accurate flux measurements with ease. The package
implements both linear and non-linear curve-fitting methodologies,
allowing you to calculate net ecosystem exchange of CO2 in `µmol/m²/s`
and the exchange of H2O due to transpiration and evapotranspiration in
`mmol/m²/s`. Furthermore, it provides Akaike Information Criterion (AIC)
scores to help you choose the most suitable fit.

### Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pkg_install("PaulESantos/co2fluxtent")
```

### LiCOR Filename Formatting

Since LiCOR measurements are often performed multiple times, the code is
designed to batch analyze many individual LiCOR runs. Currently, it is
assumed that all data to be read adheres to the following format:

**location date time of day trial.txt**

The modifications to the “trial” term are essential:

- the letter “a” represents an ambient measurement when the tent was not
  applied,

- the letters “resp” represent a measurement conducted when the tent was
  applied and shaded,

- the final scenario is the measurement when the tent was applied but
  unshaded.

It is important to note that net ecosystem productivity can be measured
directly from the standard LiCOR output. However, given the experimental
protocol of measuring water vapor concentrations during photosynthesis
and during respiration, we are, in fact, only measuring
evapotranspiration (ET) and evaporation (E), respectively. Thus, we
measure transpiration (T) as the difference between evapotranspiration
and evaporation, or T = ET - E.

### Example

``` r
df <- read_files("./inst/extdata")
data <- df |> 
  flux_calc()
data |> 
  dplyr::mutate(filename = basename(filename)) 
```

- The default calculation is ‘ET’.

``` r
# A tibble: 2 × 13
  filename               tstart tfinish  wamb   tav   pav   cav flux_lm flux_nlm lm_rsqd non_linear_sigma aic_lm aic_nlm
  <chr>                   <int>   <int> <dbl> <dbl> <dbl> <dbl>   <dbl>    <dbl>   <dbl>            <dbl>  <dbl>   <dbl>
1 06172020_almont_night…     10      50  2.30 -65.7  75.7  316.  0.0284   0.0241   0.575          0.00530  -311.   -302.
2 06172020_almont_night…     10      60  2.33 -65.7  75.7  311.  0.163    0.144    0.908          0.0145   -289.   -278.
```

- For “NEE”

``` r
data <- df |> 
  flux_calc(param = "nee")
```

``` r
# A tibble: 2 × 12
  filename  tstart tfinish  camb   tav   pav  nee_lm nee_exp lm_rsqd non_linear_sigma aic_lm aic_nlm
  <chr>      <int>   <int> <dbl> <dbl> <dbl>   <dbl>   <dbl>   <dbl>            <dbl>  <dbl>   <dbl>
1 ./inst/e…     20      80  308. -65.7  75.7 -21.8   -13.9    0.956             3.00   223.    306. 
2 ./inst/e…     10      80  305. -65.7  75.7  -0.194  -0.157  0.0369            0.364   60.9    61.2
```
