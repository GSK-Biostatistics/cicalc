# Wilson CI

Calculates the Wilson interval by calling
[`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html). Also
referred to as Wilson score interval.

## Usage

``` r
ci_prop_wilson(x, conf.level = 0.95, correct = FALSE, data = NULL)
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

\$\$\frac{\hat{p} + \frac{z^2\_{\alpha/2}}{2n} \pm z\_{\alpha/2}
\sqrt{\frac{\hat{p}(1 - \hat{p})}{n} +
\frac{z^2\_{\alpha/2}}{4n^2}}}{1 + \frac{z^2\_{\alpha/2}}{n}}\$\$
