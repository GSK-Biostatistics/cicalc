# Newcombe Confidence Interval for Difference in Proportions

Newcombe Confidence Interval for Difference in Proportions

## Usage

``` r
ci_prop_diff_nc(x, by, conf.level = 0.95, correct = FALSE, data = NULL)
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

  The number of responses for each group

- N:

  The total number in each group

- estimate:

  The point estimate of the difference in proportions

- conf.low:

  Lower bound of the confidence interval

- conf.high:

  Upper bound of the confidence interval

- conf.level:

  The confidence level used

- method:

  Anderson-Hauck Confidence Interval

## Details

The **Wilson (Score)** confidence limits without continuity correction
for each individual binomial proportion, \\p_i = x_i / n_i\\, for \\i =
1, 2\\, are given by:

\$\$ \frac{ (2 n_i \hat{p}\_i + z^2) \pm z \sqrt{ 4 n_i \hat{p}\_i (1 -
\hat{p}\_i) + z^2 } }{ 2 (n_i + z^2) } \$\$

Denote the lower and upper Wilson (Score) confidence limits for \\p_i\\
as \\L_i\\ and \\U_i\\, respectively.

Then, the **Newcombe (Score)** confidence limits for the difference in
proportions (\\p_1 - p_2\\) are given by:

\$\$ \text{Lower limit: } (\hat{p}\_1 - \hat{p}\_2) - \sqrt{
(\hat{p}\_1 - L_1)^2 + (U_2 - \hat{p}\_2)^2 } \$\$

\$\$ \text{Upper limit: } (\hat{p}\_1 - \hat{p}\_2) + \sqrt{ (U_1 -
\hat{p}\_1)^2 + (\hat{p}\_2 - L_2)^2 } \$\$

The confidence intervals with continuity correction for each individual
binomial proportion are obtained using the **Wilson (Score) confidence
limits with continuity correction**.

For each binomial proportion \\p_i = x_i / n_i\\, where \\i = 1, 2\\,
the confidence intervals are given by:

\$\$ \frac{ 2 n_i \hat{p}\_i + z^2 }{ 2 (n_i + z^2) } \\ \pm \\ \frac{ z
}{ 2 (n_i + z^2) } \sqrt{ z^2 - \frac{2}{n_i} + 4 \hat{p}\_i \left\[ n_i
(1 - \hat{p}\_i) + 1 \right\] } \$\$

## References

Newcombe, R. G. (1998). Interval estimation for the difference between
independent proportions: Comparison of eleven methods. *Statistics in
Medicine, 17*(8), 873–890. [Constructing Confidence Intervals for the
Differences of Binomial Proportions in
SAS](https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf)

## Examples

``` r
responses <- expand(c(9, 3), c(10, 10))
arm <- rep(c("treat", "control"), times = c(10, 10))

# Calculate 95% confidence interval for difference in proportions
ci_prop_diff_nc(x = responses, by = arm)
#> 
#> ── Newcombe Confidence Interval without continuity correction ──────────────────
#> • 9/10 - 3/10
#> • Estimate: 0.6
#> • 95% Confidence Interval:
#>   (0.1705, 0.809)
```
