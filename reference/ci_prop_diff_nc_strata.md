# Stratified Newcombe Common Risk Difference Confidence Interval

Calculates the stratified Newcombe confidence interval for unequal
proportions as described in Yan X, Su XG. Stratified Wilson and Newcombe
confidence intervals or multiple binomial proportions. Weights are
estimated using CMH or Wilson methods.

## Usage

``` r
ci_prop_diff_nc_strata(
  x,
  by,
  strata,
  conf.level = 0.95,
  correct = FALSE,
  weights_method = c("wilson", "cmh"),
  data = NULL
)
```

## Arguments

- x:

  (`binary`/`numeric`/`logical`)  
  vector of a binary values, i.e. a logical vector, or numeric with
  values `c(0, 1)`

- by:

  (`string`)  
  A character or factor vector with exactly two unique levels
  identifying the two groups to compare. Can also be a column name if a
  data frame provided in the `data` argument.

- strata:

  (`numeric`)  
  A vector specifying the stratum for each observation. It needs to be
  the length of x or a multiple of x if multiple levels of strata are
  present. Can also be a column name (or vector of column names NOT
  quoted) if a data frame provided in the `data` argument.

- conf.level:

  (`scalar numeric`)  
  a scalar in (0,1) indicating the confidence level. Default is 0.95

- correct:

  (scalar `logical`)  
  include the continuity correction. For further information, see for
  example \[ci_prop_diff_nc())\].

  \[ci_prop_diff_nc())\]: R:ci_prop_diff_nc())

- weights_method:

  (`character`)  
  Can be either "wilson" or "cmh" and directs the way weights are
  estimated.

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

  Weights of each strata calculated as per the specified
  "weights_method" argument.

- method:

  Type of method used

## Details

\$\$ L = \hat{d}\_{\rm MH} - z\_{\alpha/2} \sqrt{ \sum_h w_h^2 L\_{2h}
(1 - L\_{2h}) + \sum_h w_h^2 U\_{1h} (1 - U\_{1h}) } \$\$

\$\$ U = \hat{d}\_{\rm MH} + z\_{\alpha/2} \sqrt{ \sum_h w_h^2 L\_{2h}
(1 - L\_{2h}) + \sum_h w_h^2 U\_{1h} (1 - U\_{1h}) } \$\$

Where:

- \\\hat{d}\_{\rm MH}\\: Mantel-Haenszel common risk difference

- \\z\_{\alpha/2}\\: standard normal critical value

- \\w_h\\: stratum weights

- \\L\_{2h}\\, \\U\_{1h}\\: Wilson-type CI limits for stratum h

## Examples

``` r
set.seed(1)
rsp <- sample(c(TRUE, FALSE), 100, TRUE)
grp <- sample(c("Placebo", "Treatment"), 100, TRUE)
strata_data <- data.frame(
  "f1" = sample(c("a", "b"), 100, TRUE),
  "f2" = sample(c("x", "y", "z"), 100, TRUE),
  stringsAsFactors = TRUE
)
strata <- interaction(strata_data)

ci_prop_diff_nc_strata(
  x = rsp, by = grp, strata = strata, weights_method = "cmh",
  conf.level = 0.95
)
#> 
#> ── Stratified Newcombe Confidence Interval without continuity correction, CMH ──
#> • 49 responses out of 100
#> • Weights: a.x = 0.122, b.x = 0.273, a.y = 0.132, b.y = 0.14, a.z = 0.193, b.z
#> = 0.14
#> • Estimate: 0.042
#> • 95% Confidence Interval:
#>   (-0.1508, 0.2314)
```
