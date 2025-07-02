
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
#> 
#> ── Wald Confidence Interval without Continuity Correction ──────────────────────
#> • 5 responses out of 10
#> • Estimate: 0.5
#> • 90% Confidence Interval:
#>   (0.2399, 0.7601)
ci_prop_wilson(x, correct = TRUE)
#> 
#> ── Wilson Confidence Interval with continuity correction ───────────────────────
#> • 5 responses out of 10
#> • Estimate: 0.5
#> • 95% Confidence Interval:
#>   (0.2366, 0.7634)
ci_prop_clopper_pearson(x)
#> 
#> ── Clopper-Pearson Confidence Interval ─────────────────────────────────────────
#> • 5 responses out of 10
#> • Estimate: 0.5
#> • 95% Confidence Interval:
#>   (0.1871, 0.8129)
ci_prop_agresti_coull(x)
#> 
#> ── Agresti-Coull Confidence Interval ───────────────────────────────────────────
#> • 5 responses out of 10
#> • Estimate: 0.5
#> • 95% Confidence Interval:
#>   (0.2366, 0.7634)
ci_prop_jeffreys(x)
#> 
#> ── Jeffreys Interval ───────────────────────────────────────────────────────────
#> • 5 responses out of 10
#> • Estimate: 0.5
#> • 95% Confidence Interval:
#>   (0.2235, 0.7765)
# Example of CI for difference of proportions 
# Generate binary samples
responses <- expand(c(9, 3), c(10, 10))
arm <- rep(c("treat", "control"), times = c(10, 10))

# Calculate 95% confidence interval for difference in proportions
ci_prop_diff_mn(x = responses, by = arm)
#> 
#> ── Miettinen-Nurminen Confidence Interval ──────────────────────────────────────
#> • 3/10 - 9/10
#> • Estimate: -0.6
#> • 95% Confidence Interval:
#>   (-0.8406, -0.17)
```
