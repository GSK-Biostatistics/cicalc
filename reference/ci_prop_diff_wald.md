# Wald Confidence Interval for Difference in Proportions

Calculates the Wald interval by following the usual textbook definition
for a difference in proportions confidence interval using the normal
approximation.

## Usage

``` r
ci_prop_diff_wald(x, by, conf.level = 0.95, correct = FALSE, data = NULL)
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

- conf.level:

  (`scalar numeric`)  
  a scalar in (0,1) indicating the confidence level. Default is 0.95

- correct:

  (`logical`)  
  apply continuity correction.

- data:

  (`data.frame`)  
  Optional data frame containing the variables specified in `x` and
  `by`.

## Value

An object containing the following components:

- n:

  Number of responses in each by group

- N:

  Total number in each by group

- estimate:

  The point estimate of the difference in proportions (p_1 - p_2)

- conf.low:

  Lower bound of the confidence interval

- conf.high:

  Upper bound of the confidence interval

- conf.level:

  The confidence level used

- method:

  Type of method used

## Details

\$\$(\hat{p}\_1 - \hat{p}\_2) \pm z\_{\alpha/2}
\sqrt{\frac{\hat{p}\_1(1 - \hat{p}\_1)}{n_1}+\frac{\hat{p}\_2(1 -
\hat{p}\_2)}{n_2}}\$\$

## Examples

``` r
responses <- expand(c(9, 3), c(10, 10))
arm <- rep(c("treat", "control"), times = c(10, 10))

# Calculate 95% confidence interval for difference in proportions
ci_prop_diff_wald(x = responses, by = arm)
#> 
#> ── Wald Confidence Interval without Continuity Correction ──────────────────────
#> • 9/10 - 3/10
#> • Estimate: 0.6
#> • 95% Confidence Interval:
#>   (0.2605, 0.9395)
```
