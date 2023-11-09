
<!-- README.md is generated from README.Rmd. Please edit that file -->

# summarizeColByGroup

<!-- badges: start -->
<!-- badges: end -->

The goal of summarizeColByGroup is to provide a method for grouping a
data by a specified column and calculating statistics for another
specified column. With this package, users can quickly calculate
group-wise statistics like average, maximum, and minimum.

## Installation

You can install the development version of summarizeColByGroup from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stat545ubc-2023/summarizeColByGroup")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(summarizeColByGroup)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
data <- data.frame(
  group = c("a", "b", "b"),
  summary = c(1, 2, 3)
)
summarize_col_by_group(data, group_col = "group", summary_col = "summary")
#> # A tibble: 2 × 4
#>   group average max_value min_value
#>   <chr>   <dbl>     <dbl>     <dbl>
#> 1 a         1           1         1
#> 2 b         2.5         3         2
```
