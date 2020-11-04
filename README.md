
<!-- README.md is generated from README.Rmd. Please edit that file -->

# juantools

<!-- badges: start -->

<!-- badges: end -->

juantools bundles a few helper functions that I use a lot, so I am
packaging them for ease of loading\!

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("juanfung/juantools")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(juantools)
## basic example code
thou_sep(1000)
#> [1] "1000"

thou_sep(10000)
#> [1] "10 000"
```
