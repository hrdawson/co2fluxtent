
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

The goal of `co2fluxtent` is to provide the tools to analyze net
ecosystem exchange (NEE) and transpiration (T) and evapotranspiration
(ET) with their own LiCOR 7500 data.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pkg_install("PaulESantos/co2fluxtent")
```

### example

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
