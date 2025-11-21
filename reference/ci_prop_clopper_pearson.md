# Clopper-Pearson CI

Calculates the Clopper-Pearson interval by calling
[`stats::binom.test()`](https://rdrr.io/r/stats/binom.test.html). Also
referred to as the `exact` method.

## Usage

``` r
ci_prop_clopper_pearson(x, conf.level = 0.95, data = NULL)
```

## Arguments

- x:

  (`binary`/`numeric`/`logical`)  
  vector of a binary values, i.e. a logical vector, or numeric with
  values `c(0, 1)`

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

\$\$ \left( \frac{k}{n} \pm z\_{\alpha/2}
\sqrt{\frac{\frac{k}{n}(1-\frac{k}{n})}{n} +
\frac{z^2\_{\alpha/2}}{4n^2}} \right) / \left( 1 +
\frac{z^2\_{\alpha/2}}{n} \right)\$\$
