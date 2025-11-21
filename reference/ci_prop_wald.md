# Wald CI

Calculates the Wald interval by following the usual textbook definition
for a single proportion confidence interval using the normal
approximation.

## Usage

``` r
ci_prop_wald(x, conf.level = 0.95, correct = FALSE, data = NULL)
```

## Arguments

- x:

  (`binary`/`numeric`/`logical`)  
  vector of a binary values, i.e. a logical vector, or numeric with
  values `c(0, 1)`

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

- method:

  Type of method used

## Details

\$\$\hat{p} \pm z\_{\alpha/2} \sqrt{\frac{\hat{p}(1 - \hat{p})}{n}}\$\$

## Examples

``` r
# example code
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
```
