# Miettinen-Nurminen Confidence Interval for Difference in Proportions

Calculates the Miettinen-Nurminen (MN) confidence interval for the
difference between two proportions. This method can be more accurate
than traditional methods, especially with small sample sizes or
proportions close to 0 or 1.

## Usage

``` r
ci_prop_diff_mn(x, by, conf.level = 0.95, delta = NULL, data = NULL)
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

  Description of the method used ("Miettinen-Nurminen Confidence
  Interval")

If `delta` is not provided statistic and p.value will be NULL

## Details

The function implements the Miettinen-Nurminen method to compute
confidence intervals for the difference between two proportions. This
approach:

- Calculates the Miettinen-Nurminen score test statistic for different
  possible values of the proportion difference (delta)

- Identifies the delta values where the test statistic equals the
  critical value corresponding to the desired confidence level

- Returns these boundary values as the confidence interval limits

The method uses a score test with a small-sample correction factor,
making it more accurate than normal approximation methods, especially
for small samples or extreme proportions. The equation for the test
statistics is as follows:

\$\$H_0: \hat{d}-\delta \<= 0 \qquad \text{vs.} \qquad H_1:
\hat{d}-\delta \> 0\$\$

\$\$ T\_\delta = \frac{\hat{p_x} - \hat{p_y} -
\delta}{\sigma\_{mn}(\delta)}\$\$

where \\\hat{p\_\*} = s\_\*/n\_\*\\ represent the observed number of
successes divided by the number of participant in that group. The
\\\sigma\_{mn}(\delta)\\ is a function of the delta values and is create
with the following equation" \\\tilde{p\_\*}\\ represent the MLE of the
proportions. \$\$ \sigma\_{mn}(\delta) =
\sqrt{\left\[\frac{\tilde{p_y}(1-\tilde{p_y})}{n_x}+\frac{\tilde{p_x}(1-\tilde{p_x})}{n_y}
\right\]\left(\frac{N}{N-1}\right)} \$\$ \\ \tilde{p_x} =
2p\cdot{cos(a)} - \frac{L_2}{3L_3}\\ and \\ \tilde{p_y} = \tilde{p_x} +
\delta\\ where:

- \\p = \pm \sqrt{\frac{L_2^2}{(3L_3)^2} - \frac{L_1}{3L_3}}\\

- \\a = 1/3\[\pi + cos^{-1}(q/p^3)\]\\

- \\q = \frac{L_2^3}{(3L_3)^3} - \frac{L_1L_2}{6L_3^2} +
  \frac{L_0}{2L_3}\\

- \\L_3 = n_x + n_y \\

- \\L_2 = (n_x + 2 n_y)\delta - N - (s_x + s_y)\\

- \\L_1 = (n_y\delta - L_3 - 2s_y)\delta + s_x + s_y\\

- \\L_0 = s_y\delta(1-\delta)\\

For more information about these equations see Miettinen (1985)

## References

Miettinen, O. S., & Nurminen, M. (1985). Comparative analysis of two
rates. Statistics in Medicine, 4(2), 213-226.

## Examples

``` r
# Generate binary samples
responses <- expand(c(9, 3), c(10, 10))
arm <- rep(c("treat", "control"), times = c(10, 10))

# Calculate 95% confidence interval for difference in proportions
ci_prop_diff_mn(x = responses, by = arm)
#> 
#> ── Miettinen-Nurminen Confidence Interval ──────────────────────────────────────
#> • 9/10 - 3/10
#> • Estimate: 0.6
#> • 95% Confidence Interval:
#>   (0.17, 0.8406)

# Calculate 99% confidence interval
ci_prop_diff_mn(x = responses, by = arm, conf.level = 0.99)
#> 
#> ── Miettinen-Nurminen Confidence Interval ──────────────────────────────────────
#> • 9/10 - 3/10
#> • Estimate: 0.6
#> • 99% Confidence Interval:
#>   (0.0218, 0.8792)

# Calculate the p-value under the null hypothesis delta = -0.1
ci_prop_diff_mn(x = responses, by = arm, delta = -0.1)
#> 
#> ── Miettinen-Nurminen Confidence Interval ──────────────────────────────────────
#> • 9/10 - 3/10
#> • Estimate: 0.6
#> • 95% Confidence Interval:
#>   (0.17, 0.8406)
#> 
#> ── Delta 
#> • At -0.1 the statistic is 3.115 and the p-value is 9e-04

# Calculate from a data.frame
data <- data.frame(responses, arm)
ci_prop_diff_mn(x = responses, by = arm, data = data)
#> 
#> ── Miettinen-Nurminen Confidence Interval ──────────────────────────────────────
#> • 9/10 - 3/10
#> • Estimate: 0.6
#> • 95% Confidence Interval:
#>   (0.17, 0.8406)
```
