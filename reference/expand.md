# Expand Count Data into Binary Vectors

Converts count data (number of successes and total sample size) into a
binary vector of TRUE/FALSE values. This is useful for converting
summary statistics back into raw data format for analysis functions that
require individual-level data.

## Usage

``` r
expand(x, n)
```

## Arguments

- x:

  Integer (or vector of integers) representing the number of successes.

- n:

  Integer (or vector of integers) representing the total number of
  participants.

## Value

A logical vector where TRUE represents a success and FALSE represents a
failure. The length of the vector equals the sum of all sample sizes.

## Details

For each pair of values in `x` and `n`, the function creates a vector
with `x` TRUE values followed by `n-x` FALSE values. If multiple pairs
are provided, the resulting vectors are concatenated in order.

## Examples

``` r
# Convert 4 successes out of 13 participants to binary data
expand(4, 13)
#>  [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [13] FALSE

# Convert multiple groups of data
# Group 1: 9 successes out of 10
# Group 2: 3 successes out of 10
expand(c(9, 3), c(10, 10))
#>  [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE
#> [13]  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
```
