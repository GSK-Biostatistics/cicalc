
<!-- README.md is generated from README.Rmd. Please edit that file -->

# citools

<!-- badges: start -->
<!-- badges: end -->

The goal of citools is to …

## Installation

You can install the development version of citools from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("GSK-Biostatistics/citools")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(citools)
# Example of CI for proportions
x <- c(
 TRUE, TRUE, TRUE, TRUE, TRUE,
 FALSE, FALSE, FALSE, FALSE, FALSE
)
ci_prop_wald(x, conf.level = 0.9)
#> $N
#> [1] 10
#> 
#> $n
#> [1] 5
#> 
#> $estimate
#> [1] 0.5
#> 
#> $conf.low
#> [1] 0.2399258
#> 
#> $conf.high
#> [1] 0.7600742
#> 
#> $conf.level
#> [1] 0.9
#> 
#> $method
#> Wald Confidence Interval without continuity correction
ci_prop_wilson(x, correct = TRUE)
#> $N
#> [1] 10
#> 
#> $n
#> [1] 5
#> 
#> $conf.level
#> [1] 0.95
#> 
#> $estimate
#>   p 
#> 0.5 
#> 
#> $statistic
#> X-squared 
#>         0 
#> 
#> $p.value
#> [1] 1
#> 
#> $parameter
#> df 
#>  1 
#> 
#> $conf.low
#> [1] 0.2365931
#> 
#> $conf.high
#> [1] 0.7634069
#> 
#> $method
#> Wilson Confidence Interval with continuity correction
#> 
#> $alternative
#> [1] "two.sided"
ci_prop_clopper_pearson(x)
#> $N
#> [1] 10
#> 
#> $n
#> [1] 5
#> 
#> $conf.level
#> [1] 0.95
#> 
#> $estimate
#> probability of success 
#>                    0.5 
#> 
#> $statistic
#> number of successes 
#>                   5 
#> 
#> $p.value
#> [1] 1
#> 
#> $parameter
#> number of trials 
#>               10 
#> 
#> $conf.low
#> [1] 0.187086
#> 
#> $conf.high
#> [1] 0.812914
#> 
#> $method
#> [1] "Clopper-Pearson Confidence Interval"
#> 
#> $alternative
#> [1] "two.sided"
ci_prop_agresti_coull(x)
#> $N
#> [1] 10
#> 
#> $n
#> [1] 5
#> 
#> $estimate
#> [1] 0.5
#> 
#> $conf.low
#> [1] 0.2365931
#> 
#> $conf.high
#> [1] 0.7634069
#> 
#> $conf.level
#> [1] 0.95
#> 
#> $method
#> [1] "Agresti-Coull Confidence Interval"
ci_prop_jeffreys(x)
#> $N
#> [1] 10
#> 
#> $n
#> [1] 5
#> 
#> $estimate
#> [1] 0.5
#> 
#> $conf.low
#> [1] 0.2235287
#> 
#> $conf.high
#> [1] 0.7764713
#> 
#> $conf.level
#> [1] 0.95
#> 
#> $method
#> Jeffreys Interval
# Example of CI for difference of proportions 
# Generate binary samples
responses <- expand(c(9, 3), c(10, 10))
arm <- rep(c("treat", "control"), times = c(10, 10))

# Calculate 95% confidence interval for difference in proportions
ci_prop_diff_mn(x = responses, by = arm)
#> $estimate
#> [1] -0.6
#> 
#> $conf.low
#> [1] -0.8406495
#> 
#> $conf.high
#> [1] -0.170025
#> 
#> $conf.level
#> [1] 0.95
#> 
#> $delta
#> NULL
#> 
#> $statistic
#> NULL
#> 
#> $p.value
#> NULL
#> 
#> $method
#> Miettinen-Nurminen Confidence Interval
```
