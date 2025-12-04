# Mid-P CI

Calculates the exact mid-p CI for binomial proportions by inverting two
one-sided binomial tests that include the mid-p tail. This is calculated
by finding the \\P_L\\ and \\P_U\\ that satisfies the following
equations: \$\$\sum \_{x=n_1+1}^{n} \binom {n}{x}
P\_{L}^{x}(1-P\_{L})^{n-x} + \frac{1}{2} \binom{n}{n_1}
P\_{L}^{n_1}(1-P\_{L})^{n-n_1} = \alpha /2\$\$ \$\$\sum \_{x=0}^{n_1-1}
\binom {n}{x} P\_{U}^{x}(1-P\_{U})^{n-x} + \frac{1}{2} \binom{n}{n_1}
P\_{U}^{n_1}(1-P\_{U})^{n-n_1} = \alpha /2\$\$

## Usage

``` r
ci_prop_mid_p(x, conf.level = 0.95, data = NULL)
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
