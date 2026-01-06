# Mantel-Haenszel Common Risk Difference Confidence Interval

Calculates the confidence interval for the Mantel-Haenszel estimate of
the common risk difference across multiple 2x2 tables (strata), using
the Sato or Independent Binomial variance estimator.

## Usage

``` r
ci_prop_diff_mh_strata(
  x,
  by,
  strata,
  conf.level = 0.95,
  sato_var = TRUE,
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

- sato_var:

  (`logical`)  
  Use Sato variance estimate

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

  Variance estimate

- statistic:

  Z-Statistic under the null hypothesis, assuming a common risk
  difference of 0

- p.value:

  p-value under the null hypothesis, assuming a common risk difference
  of 0

- method:

  Description of the method used ("Mantel-Haenszel Confidence Interval,
  Sato Variance") or ("Mantel-Haenszel Confidence Interval, Independent
  Binomial")

## Details

The Mantel-Haenszel common risk difference is computed as:

\$\$\hat{\delta}\_{MH} = \frac{\sum\_{k} w_k \hat{\delta}\_k }{\sum\_{k}
w_k}\$\$

where \\w_k = \frac{n\_{xk} n\_{yk}}{N_k}\\, \\\hat{\delta}\_k =
s\_{xk}/n\_{xk} - y\_{yk}/n\_{yk}\\, \\N_k = n\_{xk} + n\_{yk}\\,
\\s\_{xk}\\ and \\s\_{yk}\\ are the number of events in each group, and
\\n\_{xk}\\, and \\n\_{yk}\\ are the group sizes in stratum \\k\\.

The Sato variance is:

\$\$\hat{\sigma}^2(\hat{\delta}\_{MH}) = \frac{\hat{d}\_{MH}
\sum\_{k}{P_k} + \sum_k Q_k}{\left( \sum_k w_k \right)^2}\$\$

where \\P_k = \frac{n\_{xk}^2 s\_{yk} - n\_{yk}^2 s\_{xk} + n\_{xk}
n\_{yk} (n\_{yk} - n\_{xk})/2}{N_k^2}\\ and \\Q_k =
\frac{s\_{xk}(n\_{yk} - s\_{yk}) + s\_{yk}(n\_{xk} - s\_{xk})}{2 N_k}\\.

The Cochran Independent Binomial variance is:

\$\$\hat{\sigma}^2(\hat{\delta}\_{C}) = \sum\_{k} w_k^2 \left\[
\frac{\hat{p}\_{1k}(1 - \hat{p}\_{1k})}{n\_{1k}} +
\frac{\hat{p}\_{2k}(1 - \hat{p}\_{2k})}{n\_{2k}} \right\]\$\$

where \\\hat{p}\_{1k} = \frac{s\_{xk}}{n\_{xk}}\\ and \\\hat{p}\_{2k} =
\frac{s\_{yk}}{n\_{yk}}\\.

The confidence interval is then \\\hat{\delta}\_{MH} \pm z\_{1-\alpha/2}
\sqrt{\hat{\sigma}^2(\hat{d}\_{MH})}\\.

## References

Agresti, A. (2013). Categorical Data Analysis. 3rd Edition. John Wiley &
Sons, Hoboken, NJ p. 231 Cochran, W.G. (1954). The Combination of
estimates from different experiments. Biometrics, 10(1), p.101-129

## Examples

``` r
# Generate binary samples with strata
responses <- expand(c(9, 3, 7, 2), c(10, 10, 10, 10))
arm <- rep(c("treat", "control"), 20)
strata <- rep(c("stratum1", "stratum2"), times = c(20, 20))

# Calculate common risk difference
ci_prop_diff_mh_strata(x = responses, by = arm, strata = strata)
#> 
#> ── Mantel-Haenszel Risk Difference Confidence Interval, Sato Variance ──────────
#> • 12/20 - 9/20
#> • Estimate: 0.15
#> • Variance: 0.024
#> • 95% Confidence Interval:
#>   (-0.1528, 0.4528)
# Calculate risk difference with independent binomial variance
ci_prop_diff_mh_strata(x = responses, by = arm, strata = strata, sato_var = FALSE)
#> 
#> ── Mantel-Haenszel Risk Difference Confidence Interval, Independent Binomial ───
#> • 12/20 - 9/20
#> • Estimate: 0.15
#> • Variance: 0.024
#> • 95% Confidence Interval:
#>   (-0.1521, 0.4521)
```
