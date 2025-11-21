# Agresti-Coull CI

Calculates the `Agresti-Coull` interval (created by `Alan Agresti` and
`Brent Coull`) by (for 95% CI) adding two successes and two failures to
the data and then using the Wald formula to construct a CI.

## Usage

``` r
ci_prop_agresti_coull(x, conf.level = 0.95, data = NULL)
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

\$\$ \left( \frac{\tilde{p} + z^2\_{\alpha/2}/2}{n + z^2\_{\alpha/2}}
\pm z\_{\alpha/2} \sqrt{\frac{\tilde{p}(1 - \tilde{p})}{n} +
\frac{z^2\_{\alpha/2}}{4n^2}} \right)\$\$
