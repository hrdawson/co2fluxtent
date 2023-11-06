
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
pak::pak("PaulESantos/co2fluxtent")

#or

devtools::install_github("PaulESantos/co2fluxtent")
```

### LiCOR Filename Formatting

Given that LiCOR measurements are often conducted multiple times, our
code is designed to efficiently batch analyze individual LiCOR runs. It
is currently expected that all data to be processed adheres to the
following naming format:

**./co2fluxtent/extdata/06172020_almont_night_3a.txt**

The **measurementtype** term plays a crucial role in identifying the
type of measurement, and the code assumes specific default patterns to
recognize the measurement type. It is important to note that these
patterns should be the last term in the filename. Here’s what each
pattern signifies:

- The letter **a** denotes an ambient measurement when the tent was not
  applied.

- The term **photo** represents a measurement conducted when the tent
  was applied.

- The term **resp** signifies a measurement conducted when the tent was
  applied and shaded.

By adhering to this standardized naming convention, our code streamlines
the process of batch analyzing LiCOR data, making it more efficient and
convenient for users.

### Usage

The `read_files` function serves as a crucial component in our data
analysis workflow. It is designed to streamline the process of gathering
and organizing LiCOR data for analysis. By providing a path to the data
files and specifying patterns to identify different types of
measurements (ambient, photo, resp), the function simplifies the data
retrieval process.

Here’s how to use it:

1.  Set your working directory to the folder containing your LiCOR data
    files.

2.  Call the `read_files` function, specifying the path to the data
    files and the patterns for ambient, photo, and resp measurements.

3.  The function will automatically identify and retrieve the relevant
    data files based on your specified patterns.

4.  It returns a structured list of LiCOR data files for further
    analysis.

This makes it easier to work with multiple LiCOR data files, ensuring
that you can quickly and efficiently access the data you need for your
analysis. The `read_files` function is a valuable tool for anyone
working with LiCOR data, simplifying the initial data preparation steps
in your workflow.

``` r
library(co2fluxtent)

# These data files are provided to help you get started quickly and understand the data processing workflow

licor_files <- co2fluxtent::read_files(fs::path_package(package = "co2fluxtent",
                 "extdata"))
                 
No matching photo files found.

> print(licor_files)

$photo_names
character(0)

$ambient_names
[1] "./co2fluxtent/extdata/06172020_almont_night_1a.txt"
[2] "./co2fluxtent/extdata/06172020_almont_night_3a.txt"

$resp_names
[1] "./co2fluxtent/extdata/06172020_almont_night_1resp.txt"
[2] "./co2fluxtent/extdata/06172020_almont_night_3resp.txt"
                 
                 
```

The next step is to utilize the `flux_calc()` function, a valuable tool
for analyzing LiCOR data. This function takes the results obtained from
the `read_files()` function as its input, seamlessly integrating data
processing into your analysis workflow. With `flux_calc()`, you can
perform both linear and non-linear fitting to assess Net Ecosystem
Exchange (NEE) or Evapotranspiration (ET) data, providing invaluable
insights into carbon dioxide and water vapor flux dynamics. To select
the parameter for calculation, you can modify the `param` argument, with
the default set to **“et”**. The `skip` argument allows you to skip the
first ‘n’ rows of data, and you can specify the volume of the chamber
using the `vol` argument. Additionally, the `area` argument represents
the chamber’s area in square meters.

``` r
licor_data <- licor_files |> 
  co2fluxtent::flux_calc(param = "nee", 
                         skip = 9,
                         vol = 2.197, 
                         area = 1.69)
  
data |> 
  dplyr::mutate(filename = basename(filename)) 
```

While the `flux_calc()` function is in operation, it will prompt the
user to specify the start and end times for conducting the curve
fitting. This interactive feature empowers the user to exclude
potentially transient data patterns detected during the data collection
process. After the user selects the desired time range, both fitting
procedures are executed automatically, and the results are promptly
displayed in the console window. The output of the `flux_calc()`
function is a tibble with the following columns:

- filename: The name of the file

- tstart: The start time of the measurement

- tfinish: The end time of the measurement

- camb: The ambient CO2 concentration

- tav: The ambient air temperature

- pav: The ambient air pressure

- nee_lm: The linear model fit of the NEE data

- nee_exp: The non-linear model fit of the NEE data

- lm_rsqd: The R-squared value of the linear model fit

- non_linear_sigma: The sigma value of the non-linear model fit

- aic_lm: The AIC score of the linear model fit

- aic_nlm: The AIC score of the non-linear model fit

``` r
# A tibble: 2 × 12
  filename                         tstart tfinish  camb   tav   pav  nee_lm nee_exp lm_rsqd non_linear_sigma aic_lm aic_nlm
  <chr>                             <int>   <int> <dbl> <dbl> <dbl>   <dbl>   <dbl>   <dbl>        <dbl>      <dbl>   <dbl>
1 06172020_almont_night_1resp.txt    20      60    308. -65.7  75.7  -25.5   -12.0    0.952         3.23       137.    211. 
2 06172020_almont_night_3resp.txt    10      80    305. -65.7  75.7  -0.194  -0.157   0.0369        0.364      60.9    61.2
```

### Citation

To cite the `co2fluxtent` package, please use:

``` r
citation("co2fluxtent")
#> To cite package 'co2fluxtent' in publications use:
#> 
#>   Brummer A, Enquist B, Santos-Andrade P (2023). _co2fluxtent: Tools
#>   for NEE and ET Fitting from CO2 Flux_. R package version 0.0.2,
#>   <https://github.com/PaulESantos/co2fluxtent>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {co2fluxtent: Tools for NEE and ET Fitting from CO2 Flux},
#>     author = {Alexander B. Brummer and Brian J. Enquist and Paul Efren Santos-Andrade},
#>     year = {2023},
#>     note = {R package version 0.0.2},
#>     url = {https://github.com/PaulESantos/co2fluxtent},
#>   }
```
