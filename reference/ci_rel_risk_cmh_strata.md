# Mantel-Haenszel Stratified Relative Risk Confidence Interval

Calculates the confidence interval for the Mantel-Haenszel estimate of
the common relative risk across multiple 2x2 tables (strata)

## Usage

``` r
ci_rel_risk_cmh_strata(x, by, strata, conf.level = 0.95, data = NULL)
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

- data:

  (`data.frame`)  
  Optional data frame containing the variables specified in `x` and
  `by`.

## Value

An object containing the following components:

- estimate:

  The Mantel-Haenszel estimated common risk difference

- conf.low:

  Lower bound of the confidence interval

- conf.high:

  Upper bound of the confidence interval

- conf.level:

  The confidence level used

- variance:

  Mantel-Haenszel variance estimate \\Var(log(RR_MH))\\

- method:

  Description of the method used ("Mantel-Haenszel Common Relative Risk
  Confidence Interval")

## Details

The Mantel-Haenszel relative risk difference is computed as:

\$\$RR\_{MH} = \frac{\sum\_{k} s\_{xk}~n\_{yk}/N_k}{\sum\_{k}
s\_{yk}~n\_{xk}/N_k}\$\$

The variance is:

\$\$\hat{\sigma}^2 = \hat{Var}(log(RR\_{MH})) =
\frac{\sum\_{k}(n\_{xk}~n\_{yk}~(s\_{xk}+s\_{yk}) -
s\_{xk}~s\_{yk}~N_k)/N_k^2}
{(\sum\_{k}s\_{xk}~n\_{yk}/N_k)(\sum\_{k}s\_{yk}~n\_{xk}/N_k)}\$\$

The confidence interval is then \\\left(RR\_{MH}\times
exp(-z\_{1-\alpha/2} \sqrt{\hat{\sigma}^2}, RR\_{MH}\times
exp(z\_{1-\alpha/2} \sqrt{\hat{\sigma}^2}\right)\\.

## References

Agresti, A. (2013). Categorical Data Analysis. 3rd Edition. John Wiley &
Sons, Hoboken, NJ

## Examples

``` r
# Generate binary samples with strata
responses <- expand(c(9, 3, 7, 2), c(10, 10, 10, 10))
arm <- rep(c("treat", "control"), 20)
strata <- rep(c("stratum1", "stratum2"), times = c(20, 20))

# Calculate common risk difference
ci_rel_risk_cmh_strata(x = responses, by = arm, strata = strata)
#> 
#> ── Mantel-Haenszel Common Relattive Risk Confidence Interval ───────────────────
#> • 12/20 - 9/20
#> • Estimate: 1.333
#> • Variance: 0.093
#> • 95% Confidence Interval:
#>   (0.7344, 2.4208)
```
