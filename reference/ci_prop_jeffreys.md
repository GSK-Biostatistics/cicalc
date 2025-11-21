# Jeffreys CI

Calculates the Jeffreys interval, an equal-tailed interval based on the
non-informative Jeffreys prior for a binomial proportion.

## Usage

``` r
ci_prop_jeffreys(x, conf.level = 0.95, data = NULL)
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

\$\$\left( \text{Beta}\left(\frac{k}{2} + \frac{1}{2}, \frac{n - k}{2} +
\frac{1}{2}\right)\_\alpha, \text{Beta}\left(\frac{k}{2} + \frac{1}{2},
\frac{n - k}{2} + \frac{1}{2}\right)\_{1-\alpha} \right)\$\$
