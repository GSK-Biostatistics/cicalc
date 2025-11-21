# Anderson-Hauck Confidence Interval for Difference in Proportions

Anderson-Hauck Confidence Interval for Difference in Proportions

## Usage

``` r
ci_prop_diff_ha(x, by, conf.level = 0.95, data = NULL)
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

The confidence interval is given by:

\$\$(\hat{p}\_1 - \hat{p}\_2) \pm \left\[ \frac{1}{2 \min(n_1, n_2)} + z
\sqrt{ \frac{\hat{p}\_1 (1 - \hat{p}\_1)}{n_1 - 1} + \frac{\hat{p}\_2
(1 - \hat{p}\_2)}{n_2 - 1} } \right\]\$\$.

## References

Hauck WW, Anderson S. (1986) A comparison of large-sample confidence
interval methods for the difference of two binomial probabilities The
American Statistician 40(4). p.318-322. [Constructing Confidence
Intervals for the Differences of Binomial Proportions in
SAS](https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf)

## Examples

``` r
responses <- expand(c(9, 3), c(10, 10))
arm <- rep(c("treat", "control"), times = c(10, 10))

# Calculate 95% confidence interval for difference in proportions
ci_prop_diff_ha(x = responses, by = arm)
#> 
#> ── Anderson-Hauck Confidence Interval ──────────────────────────────────────────
#> • 9/10 - 3/10
#> • Estimate: 0.6
#> • 95% Confidence Interval:
#>   (0.1922, 1)
```
