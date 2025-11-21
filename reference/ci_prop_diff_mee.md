# Mee Confidence Interval for Difference in Proportions

Mee Confidence Interval for Difference in Proportions

## Usage

``` r
ci_prop_diff_mee(x, by, conf.level = 0.95, delta = NULL, data = NULL)
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

- n:

  The number of responses for each group

- N:

  The total number in each group

- estimate:

  The point estimate of the difference in proportions (p1-p2)

- conf.low:

  Lower bound of the confidence interval

- conf.high:

  Upper bound of the confidence interval

- conf.level:

  The confidence level used

- method:

  Mee Confidence Interval

## Details

The confidence interval is calculated by \\\theta^\* \pm w\\ where:

\$\$\theta^\* = \frac{(\hat{p}\_1 - \hat{p}\_2) +
z^2v(1-2\hat{\psi})}{1+z^2u}\$\$ where \$\$w =
\frac{z}{1+z^2u}\sqrt{u\\4\hat{\psi}(1-\hat{\psi})-(\hat{p}\_1 -
\hat{p}\_2)^2\\+2v(1-2\hat{\psi})(\hat{p}\_1-\hat{p}\_2)
+4z^2v^2(1-2\hat{\psi})^2 }\$\$ \$\$\hat{\psi} =
\frac{1}{2}\left(\frac{x_1 + 1/2}{n_1+1}+\frac{x_2 +
1/2}{n_2+1}\right)\$\$ \$\$u = \frac{1/n_1 + 1/n_2}{4}\$\$ \$\$v =
\frac{1/n_1 - 1/n_2}{4}\$\$

## References

[Constructing Confidence Intervals for the Differences of Binomial
Proportions in
SAS](https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf)

## Examples

``` r
responses <- expand(c(9, 3), c(10, 10))
arm <- rep(c("treat", "control"), times = c(10, 10))

# Calculate 95% confidence interval for difference in proportions
ci_prop_diff_mee(x = responses, by = arm)
#> 
#> ── Mee Confidence Interval ─────────────────────────────────────────────────────
#> • 9/10 - 3/10
#> • Estimate: 0.6
#> • 95% Confidence Interval:
#>   (0.1821, 0.837)
```
