
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gtrendsAPI

<!-- badges: start -->
<!-- badges: end -->

The goal of gtrendsAPI is to provides a platform to access data from the
official Google Trends API through R.

## Installation

You can install the development version of gtrendsAPI from GitHub like
so:

``` r
# install.packages("devtools")
devtools::install_github("racorreia/gkgraphR", build_vignettes = T)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# Load the gtrendsAPI package
library(gtrendsAPI)
# Query the Google Trends API for worldwide search data on the word 'apple'
# apple_sv <- getGraph("apple", api.key = "YOUR_API_KEY")
```

Examples of the use of each function available in this package are
provided in the function help, which can be accessed like so:

``` r
# Get function help
?getGraph
```
