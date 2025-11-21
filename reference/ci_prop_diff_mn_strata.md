# Stratified Miettinen-Nurminen Confidence Interval for Difference in Proportions

Calculates Stratified Miettinen-Nurminen (MN) confidence intervals and
corresponding point estimates for the difference between two proportions

## Usage

``` r
ci_prop_diff_mn_strata(
  x,
  by,
  strata,
  method = c("score", "summary score"),
  conf.level = 0.95,
  delta = NULL,
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

- method:

  (`string`)  
  Specifying how the CIs should be calculated. It must equal either
  'score' or 'summary score'. See details for more information about the
  implementation differences.

- conf.level:

  (`scalar numeric`)  
  a scalar in (0,1) indicating the confidence level. Default is 0.95

- delta:

  (`numeric`)  
  Optionally a single number or a vector of numbers between -1 and 1
  (not inclusive) to set the difference between two groups under the
  null hypothesis. If provided, the function returns the test statistic
  and p-value under the `delta` hypothesis.

- data:

  (`data.frame`)  
  Optional data frame containing the variables specified in `x` and
  `by`.

## Value

An object containing the following components:

- estimate:

  The point estimate of the difference in proportions (p_x - p_y)

- conf.low:

  Lower bound of the confidence interval

- conf.high:

  Upper bound of the confidence interval

- conf.level:

  The confidence level used

- delta:

  delta value(s) used

- statistic:

  Z-Statistic under the null hypothesis based on the given 'delta'

- p.value:

  p-value under the null hypothesis based on the given 'delta'

- method:

  Description of the method used ("Stratified {method}
  Miettinen-Nurminen Confidence Interval")

If `delta` is not provided statistic and p.value will be NULL

## Details

The function implements the stratified Miettinen-Nurminen method to
compute confidence intervals for the difference between two proportions
across multiple strata. \$\$H_0: \hat{d}-\delta \<= 0 \qquad \text{vs.}
\qquad H_1: \hat{d}-\delta \> 0\$\$

The "score" method is a weighted MN score first described in the
original 1985 paper. The formula is:

- Calculates weights for each stratum as \\w_i = \frac{n\_{xi} \cdot
  n\_{yi}}{n\_{xi} + n\_{yi}}\\

- Computes the overall weighted difference \\\hat{d} = \frac{\sum w_i
  \hat{p}\_{xi}}{\sum w_i} - \frac{\sum w_i \hat{p}\_{yi}}{\sum w_i}\\

- Uses the stratified test statistic: \$\$Z\_{\delta} = \frac{\hat{d} -
  \delta} {\sqrt{\sum\_{i=1}^k \left(\frac{w_i}{\sum w_i}\right)^2 \cdot
  \hat{\sigma}\_{mn}^2({d})}}\$\$

- Finds the range of all values of \\\delta\\ for which the stratified
  test statistic (\\Z\_\delta\\) falls in the acceptance region \\\\
  Z\_\delta \< z\_{\alpha/2}\\\\

The \\\hat{\sigma}\_{mn}^2(\hat{d})\\ is the Miettinen-Nurminen variance
estimate. See the details of
[`ci_prop_diff_mn()`](https://gsk-biostatistics.github.io/cicalc/reference/ci_prop_diff_mn.md)
for how \\\hat{\sigma}\_{mn}^2(\delta)\\ is calculated.

The "summary score" method follows the meta-analyses proposed in Agresti
2013 and is consistent with the "Summary Score Confidence Limits" method
used in SAS. The formula is:

- The point estimate of the stratified risk difference is a weighted
  average of the midpoints of the within-stratum MN confidence
  intervals: \$\$ \hat{d}\_{\text{S}} = \sum_i \hat{d}\_i w_i \$\$

- Define \\s_i\\ as the width of the CI for the \\i\\th stratum divided
  by \\2 \times z\_{\alpha/2}\\ and then stratum weights are given by
  \$\$ w_i = \left( \frac{1}{s_i^2} \right) \bigg/ \sum_i \left(
  \frac{1}{s_i^2} \right) \$\$

- The variance of \\\hat{d}\_{\text{S}} \\ is computed as \$\$
  \widehat{\text{Var}}(\hat{d}\_{\text{S}}) = \frac{1}{\sum_i \left(
  \frac{1}{s_i^2} \right) } \$\$

- Confidence limits for the stratified risk difference estimate are \$\$
  \hat{d}\_{\text{S}} \pm \left( z\_{\alpha /2} \times
  \widehat{\text{Var}}(\hat{d}\_{\text{S}}) \right) \$\$

## References

Miettinen, O. S., & Nurminen, M. (1985). Comparative analysis of two
rates. Statistics in Medicine, 4(2), 213-226.

[Common Risk Difference :: Base SAS(R) 9.4 Procedures Guide: Statistical
Procedures, Third
Edition](https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/procstat/procstat_freq_details63.htm)

Agresti, A. (2013). Categorical Data Analysis. 3rd Edition. John Wiley &
Sons, Hoboken, NJ

## Examples

``` r
# Generate binary samples with strata
responses <- expand(c(9, 3, 7, 2), c(10, 10, 10, 10))
arm <- rep(c("treat", "control"), 20)
strata <- rep(c("stratum1", "stratum2"), times = c(20, 20))

# Calculate stratified confidence interval for difference in proportions
ci_prop_diff_mn_strata(x = responses, by = arm, strata = strata)
#> 
#> ── Stratified Score Miettinen-Nurminen Confidence Interval ─────────────────────
#> • 12/20 - 9/20
#> • Weights: stratum1 = 5, stratum2 = 5
#> • Estimate: 0.15
#> • 95% Confidence Interval:
#>   (-0.1606, 0.4338)

# Using the summary score method
ci_prop_diff_mn_strata(x = responses, by = arm, strata = strata,
                      method = "summary score")
#> 
#> ── Stratified Summary Score Miettinen-Nurminen Confidence Interval ─────────────
#> • 12/20 - 9/20
#> • Weights: stratum1 = 0.511, stratum2 = 0.489
#> • Estimate: -0.126
#> • 95% Confidence Interval:
#>   (-0.4113, 0.1586)

# Calculate 99% confidence interval
ci_prop_diff_mn_strata(x = responses, by = arm, strata = strata,
                      conf.level = 0.99)
#> 
#> ── Stratified Score Miettinen-Nurminen Confidence Interval ─────────────────────
#> • 12/20 - 9/20
#> • Weights: stratum1 = 5, stratum2 = 5
#> • Estimate: 0.15
#> • 99% Confidence Interval:
#>   (-0.2509, 0.5072)

# Calculate p-value under null hypothesis delta = 0.2
ci_prop_diff_mn_strata(x = responses, by = arm, strata = strata,
                      delta = 0.2)
#> 
#> ── Stratified Score Miettinen-Nurminen Confidence Interval ─────────────────────
#> • 12/20 - 9/20
#> • Weights: stratum1 = 5, stratum2 = 5
#> • Estimate: 0.15
#> • 95% Confidence Interval:
#>   (-0.1606, 0.4338)
#> 
#> ── Delta 
#> • At 0.2 the statistic is -0.319 and the p-value is 0.375
```
