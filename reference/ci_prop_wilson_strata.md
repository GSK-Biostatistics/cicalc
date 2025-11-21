# Stratified Wilson CI

Calculates the stratified Wilson confidence interval for unequal
proportions as described in Xin YA, Su XG. Stratified Wilson and
Newcombe confidence intervals for multiple binomial proportions.
*Statistics in Biopharmaceutical Research*. 2010;2(3).

## Usage

``` r
ci_prop_wilson_strata(
  x,
  strata,
  weights = NULL,
  conf.level = 0.95,
  max.iterations = 10L,
  correct = FALSE,
  data = NULL
)
```

## Arguments

- x:

  (`binary`/`numeric`/`logical`)  
  vector of a binary values, i.e. a logical vector, or numeric with
  values `c(0, 1)`

- strata:

  (`numeric`)  
  A vector specifying the stratum for each observation. It needs to be
  the length of x or a multiple of x if multiple levels of strata are
  present. Can also be a column name (or vector of column names NOT
  quoted) if a data frame provided in the `data` argument.

- weights:

  (`numeric`)  
  weights for each level of the strata. If `NULL`, they are estimated
  using the iterative algorithm that minimizes the weighted squared
  length of the confidence interval.

- conf.level:

  (`scalar numeric`)  
  a scalar in (0,1) indicating the confidence level. Default is 0.95

- max.iterations:

  (positive `integer`)  
  maximum number of iterations for the iterative procedure used to find
  estimates of optimal weights.

- correct:

  (scalar `logical`)  
  include the continuity correction. For further information, see for
  example
  [`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html).

- data:

  (`data.frame`)  
  Optional data frame containing the variables specified in `x` and
  `by`.

## Value

An object containing the following components:

- n:

  Number of responses

- N:

  Total number

- estimate:

  The point estimate of the proportion

- conf.low:

  Lower bound of the confidence interval

- conf.high:

  Upper bound of the confidence interval

- conf.level:

  The confidence level used

- weights:

  Weights of each strata, will be the same as the input unless
  unspecified, then it will be the dynamically calculated weights.

- method:

  Type of method used

## Details

\$\$\frac{\hat{p}\_j + \frac{z^2\_{\alpha/2}}{2n_j} \pm z\_{\alpha/2}
\sqrt{\frac{\hat{p}\_j(1 - \hat{p}\_j)}{n_j} +
\frac{z^2\_{\alpha/2}}{4n_j^2}}}{1 + \frac{z^2\_{\alpha/2}}{n_j}}\$\$

## Examples

``` r
# Stratified Wilson confidence interval with unequal probabilities

set.seed(1)
rsp <- sample(c(TRUE, FALSE), 100, TRUE)
strata_data <- data.frame(
  "f1" = sample(c("a", "b"), 100, TRUE),
  "f2" = sample(c("x", "y", "z"), 100, TRUE),
  stringsAsFactors = TRUE
)
strata <- interaction(strata_data)
n_strata <- ncol(table(rsp, strata)) # Number of strata

ci_prop_wilson_strata(
  x = rsp, strata = strata,
  conf.level = 0.90
)
#> 
#> ── Stratified Wilson Confidence Interval without continuity correction ─────────
#> • 49 responses out of 100
#> • Weights: a.x = 0.207, b.x = 0.178, a.y = 0.192, b.y = 0.16, a.z = 0.135, b.z
#> = 0.128
#> • Estimate: 0.49
#> • 90% Confidence Interval:
#>   (0.4073, 0.5648)

# Not automatic setting of weights
ci_prop_wilson_strata(
  x = rsp, strata = strata,
  weights = rep(1 / n_strata, n_strata),
  conf.level = 0.90
)
#> 
#> ── Stratified Wilson Confidence Interval without continuity correction ─────────
#> • 49 responses out of 100
#> • Weights: 0.167, 0.167, 0.167, 0.167, 0.167, 0.167
#> • Estimate: 0.49
#> • 90% Confidence Interval:
#>   (0.419, 0.579)
```
