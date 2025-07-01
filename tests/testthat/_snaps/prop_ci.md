# check the ci_prop_*() functions work

    Code
      wilson_dbl <- ci_prop_wilson(x_dbl, conf.level = 0.9, correct = FALSE)

---

    Code
      wilsoncc_dbl <- ci_prop_wilson(x_dbl, conf.level = 0.9, correct = TRUE)

---

    Code
      wilson_lgl <- ci_prop_wilson(x_lgl, conf.level = 0.9, correct = FALSE)

---

    Code
      ci_prop_wilson(x_rsp, conf.level = 0.9)
    Output
      $N
      [1] 10
      
      $n
      [1] 5
      
      $conf.level
      [1] 0.9
      
      $estimate
        p 
      0.5 
      
      $statistic
      X-squared 
              0 
      
      $p.value
      [1] 1
      
      $parameter
      df 
       1 
      
      $conf.low
      [1] 0.2692718
      
      $conf.high
      [1] 0.7307282
      
      $method
      Wilson Confidence Interval without continuity correction
      
      $alternative
      [1] "two.sided"
      
      attr(,"class")
      [1] "wilson"      "prop_ci_uni" "cicada"     

---

    Code
      ci_prop_wilson(x_true)
    Output
      $N
      [1] 32
      
      $n
      [1] 32
      
      $conf.level
      [1] 0.95
      
      $estimate
      p 
      1 
      
      $statistic
      X-squared 
             32 
      
      $p.value
      [1] 1.541726e-08
      
      $parameter
      df 
       1 
      
      $conf.low
      [1] 0.8928208
      
      $conf.high
      [1] 1
      
      $method
      Wilson Confidence Interval without continuity correction
      
      $alternative
      [1] "two.sided"
      
      attr(,"class")
      [1] "wilson"      "prop_ci_uni" "cicada"     

---

    Code
      ci_prop_wilson(x_false)
    Output
      $N
      [1] 32
      
      $n
      [1] 0
      
      $conf.level
      [1] 0.95
      
      $estimate
      p 
      0 
      
      $statistic
      X-squared 
             32 
      
      $p.value
      [1] 1.541726e-08
      
      $parameter
      df 
       1 
      
      $conf.low
      [1] 0
      
      $conf.high
      [1] 0.1071792
      
      $method
      Wilson Confidence Interval without continuity correction
      
      $alternative
      [1] "two.sided"
      
      attr(,"class")
      [1] "wilson"      "prop_ci_uni" "cicada"     

---

    Code
      wald_dbl <- ci_prop_wald(x_dbl, conf.level = 0.9, correct = FALSE)

---

    Code
      waldcc_dbl <- ci_prop_wald(x_dbl, conf.level = 0.9, correct = TRUE)

---

    Code
      wald_lgl <- ci_prop_wald(x_lgl, conf.level = 0.9, correct = FALSE)

---

    Code
      ci_prop_wald(x_rsp, conf.level = 0.95, correct = TRUE)
    Output
      $N
      [1] 10
      
      $n
      [1] 5
      
      $estimate
      [1] 0.5
      
      $conf.low
      [1] 0.1401025
      
      $conf.high
      [1] 0.8598975
      
      $conf.level
      [1] 0.95
      
      $method
      Wald Confidence Interval with continuity correction
      
      attr(,"class")
      [1] "wald"        "prop_ci_uni" "cicada"     

---

    Code
      ci_prop_wald(x_true)
    Output
      $N
      [1] 32
      
      $n
      [1] 32
      
      $estimate
      [1] 1
      
      $conf.low
      [1] 1
      
      $conf.high
      [1] 1
      
      $conf.level
      [1] 0.95
      
      $method
      Wald Confidence Interval without continuity correction
      
      attr(,"class")
      [1] "wald"        "prop_ci_uni" "cicada"     

---

    Code
      ci_prop_wald(x_false)
    Output
      $N
      [1] 32
      
      $n
      [1] 0
      
      $estimate
      [1] 0
      
      $conf.low
      [1] 0
      
      $conf.high
      [1] 0
      
      $conf.level
      [1] 0.95
      
      $method
      Wald Confidence Interval without continuity correction
      
      attr(,"class")
      [1] "wald"        "prop_ci_uni" "cicada"     

---

    Code
      clopper_pearson_dbl <- ci_prop_clopper_pearson(x_dbl, conf.level = 0.9)

---

    Code
      clopper_pearson_lgl <- ci_prop_clopper_pearson(x_lgl, conf.level = 0.9)

---

    Code
      ci_prop_clopper_pearson(x_rsp, conf.level = 0.95)
    Output
      $N
      [1] 10
      
      $n
      [1] 5
      
      $conf.level
      [1] 0.95
      
      $estimate
      probability of success 
                         0.5 
      
      $statistic
      number of successes 
                        5 
      
      $p.value
      [1] 1
      
      $parameter
      number of trials 
                    10 
      
      $conf.low
      [1] 0.187086
      
      $conf.high
      [1] 0.812914
      
      $method
      [1] "Clopper-Pearson Confidence Interval"
      
      $alternative
      [1] "two.sided"
      
      attr(,"class")
      [1] "clopper-pearson" "prop_ci_uni"     "cicada"         

---

    Code
      ci_prop_wilson(x_true)
    Output
      $N
      [1] 32
      
      $n
      [1] 32
      
      $conf.level
      [1] 0.95
      
      $estimate
      p 
      1 
      
      $statistic
      X-squared 
             32 
      
      $p.value
      [1] 1.541726e-08
      
      $parameter
      df 
       1 
      
      $conf.low
      [1] 0.8928208
      
      $conf.high
      [1] 1
      
      $method
      Wilson Confidence Interval without continuity correction
      
      $alternative
      [1] "two.sided"
      
      attr(,"class")
      [1] "wilson"      "prop_ci_uni" "cicada"     

---

    Code
      ci_prop_wilson(x_false)
    Output
      $N
      [1] 32
      
      $n
      [1] 0
      
      $conf.level
      [1] 0.95
      
      $estimate
      p 
      0 
      
      $statistic
      X-squared 
             32 
      
      $p.value
      [1] 1.541726e-08
      
      $parameter
      df 
       1 
      
      $conf.low
      [1] 0
      
      $conf.high
      [1] 0.1071792
      
      $method
      Wilson Confidence Interval without continuity correction
      
      $alternative
      [1] "two.sided"
      
      attr(,"class")
      [1] "wilson"      "prop_ci_uni" "cicada"     

---

    Code
      agresti_coull_dbl <- ci_prop_agresti_coull(x_dbl, conf.level = 0.9)

---

    Code
      agresti_coull_lgl <- ci_prop_agresti_coull(x_lgl, conf.level = 0.9)

---

    Code
      ci_prop_agresti_coull(x_rsp, conf.level = 0.95)
    Output
      $N
      [1] 10
      
      $n
      [1] 5
      
      $estimate
      [1] 0.5
      
      $conf.low
      [1] 0.2365931
      
      $conf.high
      [1] 0.7634069
      
      $conf.level
      [1] 0.95
      
      $method
      [1] "Agresti-Coull Confidence Interval"
      
      attr(,"class")
      [1] "agresti-coull" "prop_ci_uni"   "cicada"       

---

    Code
      ci_prop_agresti_coull(x_true)
    Output
      $N
      [1] 32
      
      $n
      [1] 32
      
      $estimate
      [1] 1
      
      $conf.low
      [1] 0.8726819
      
      $conf.high
      [1] 1
      
      $conf.level
      [1] 0.95
      
      $method
      [1] "Agresti-Coull Confidence Interval"
      
      attr(,"class")
      [1] "agresti-coull" "prop_ci_uni"   "cicada"       

---

    Code
      ci_prop_agresti_coull(x_false)
    Output
      $N
      [1] 32
      
      $n
      [1] 0
      
      $estimate
      [1] 0
      
      $conf.low
      [1] 0
      
      $conf.high
      [1] 0.1273181
      
      $conf.level
      [1] 0.95
      
      $method
      [1] "Agresti-Coull Confidence Interval"
      
      attr(,"class")
      [1] "agresti-coull" "prop_ci_uni"   "cicada"       

---

    Code
      jeffreys_dbl <- ci_prop_jeffreys(x_dbl, conf.level = 0.9)

---

    Code
      jeffreys_lgl <- ci_prop_jeffreys(x_lgl, conf.level = 0.9)

---

    Code
      ci_prop_jeffreys(x_rsp, conf.level = 0.95)
    Output
      $N
      [1] 10
      
      $n
      [1] 5
      
      $estimate
      [1] 0.5
      
      $conf.low
      [1] 0.2235287
      
      $conf.high
      [1] 0.7764713
      
      $conf.level
      [1] 0.95
      
      $method
      Jeffreys Interval
      
      attr(,"class")
      [1] "jeffreys"    "prop_ci_uni" "cicada"     

---

    Code
      ci_prop_jeffreys(x_true)
    Output
      $N
      [1] 32
      
      $n
      [1] 32
      
      $estimate
      [1] 1
      
      $conf.low
      [1] 0.9250722
      
      $conf.high
      [1] 1
      
      $conf.level
      [1] 0.95
      
      $method
      Jeffreys Interval
      
      attr(,"class")
      [1] "jeffreys"    "prop_ci_uni" "cicada"     

---

    Code
      ci_prop_jeffreys(x_false)
    Output
      $N
      [1] 32
      
      $n
      [1] 0
      
      $estimate
      [1] 0
      
      $conf.low
      [1] 0
      
      $conf.high
      [1] 0.07492776
      
      $conf.level
      [1] 0.95
      
      $method
      Jeffreys Interval
      
      attr(,"class")
      [1] "jeffreys"    "prop_ci_uni" "cicada"     

---

    Code
      ci_prop_wilson(x_dbl, conf.level = c(0.9, 0.9))
    Condition
      Error in `ci_prop_wilson()`:
      ! The `conf.level` argument must be length 1.

---

    Code
      ci_prop_wilson(mtcars$cyl)
    Condition
      Error in `ci_prop_wilson()`:
      ! Expecting `x` to be either <logical> or <numeric/integer> coded as 0 and 1.

# check the ci_prop_strat_wilson() function works

    Code
      ci_prop_wilson_strata(x = rsp, strata = strata, weights = weights, correct = FALSE)
    Output
      $N
      [1] 80
      
      $n
      [1] 50
      
      $estimate
      [1] 0.625
      
      $conf.low
      [1] 0.4867191
      
      $conf.high
      [1] 0.7186381
      
      $conf.level
      [1] 0.95
      
      $weights
      [1] 0.04761905 0.09523810 0.14285714 0.19047619 0.23809524 0.28571429
      
      $method
      Stratified Wilson Confidence Interval without continuity correction
      
      attr(,"class")
      [1] "stratified-wilson" "prop_ci_uni"       "cicada"           

---

    Code
      ci_prop_wilson_strata(x = rsp, strata = strata, weights = weights, correct = TRUE)
    Output
      $N
      [1] 80
      
      $n
      [1] 50
      
      $estimate
      [1] 0.625
      
      $conf.low
      [1] 0.4482566
      
      $conf.high
      [1] 0.7531474
      
      $conf.level
      [1] 0.95
      
      $weights
      [1] 0.04761905 0.09523810 0.14285714 0.19047619 0.23809524 0.28571429
      
      $method
      Stratified Wilson Confidence Interval with continuity correction
      
      attr(,"class")
      [1] "stratified-wilson" "prop_ci_uni"       "cicada"           

---

    Code
      ci_prop_wilson_strata(x = as.numeric(rsp), strata = strata, weights = weights)
    Output
      $N
      [1] 80
      
      $n
      [1] 50
      
      $estimate
      [1] 0.625
      
      $conf.low
      [1] 0.4867191
      
      $conf.high
      [1] 0.7186381
      
      $conf.level
      [1] 0.95
      
      $weights
      [1] 0.04761905 0.09523810 0.14285714 0.19047619 0.23809524 0.28571429
      
      $method
      Stratified Wilson Confidence Interval without continuity correction
      
      attr(,"class")
      [1] "stratified-wilson" "prop_ci_uni"       "cicada"           

---

    Code
      ci_prop_wilson_strata(x = as.numeric(rsp), strata = strata)
    Output
      $N
      [1] 80
      
      $n
      [1] 50
      
      $estimate
      [1] 0.625
      
      $conf.low
      [1] 0.5242016
      
      $conf.high
      [1] 0.7268788
      
      $conf.level
      [1] 0.95
      
      $weights
            a.x       b.x       a.y       b.y       a.z       b.z 
      0.2111332 0.1890860 0.1180990 0.1544903 0.1737106 0.1534809 
      
      $method
      Stratified Wilson Confidence Interval without continuity correction
      
      attr(,"class")
      [1] "stratified-wilson" "prop_ci_uni"       "cicada"           

---

    Code
      ci_prop_wilson_strata(x = rep_len(TRUE, length(rsp)), strata = strata, weights = weights)
    Condition
      Error in `ci_prop_wilson_strata()`:
      ! All values in `x` argument are either `TRUE` or `FALSE` and CI is not estimable.

---

    Code
      ci_prop_wilson_strata(x = rep_len(FALSE, length(rsp)), strata = strata,
      weights = weights)
    Condition
      Error in `ci_prop_wilson_strata()`:
      ! All values in `x` argument are either `TRUE` or `FALSE` and CI is not estimable.

---

    Code
      ci_prop_wilson_strata(x = as.numeric(rsp), strata = strata, max.iterations = -1)
    Condition
      Error in `ci_prop_wilson_strata()`:
      ! Argument `max.iterations` must be a positive integer.

---

    Code
      ci_prop_wilson_strata(x = as.numeric(rsp), strata = strata, max.iterations = -1)
    Condition
      Error in `ci_prop_wilson_strata()`:
      ! Argument `max.iterations` must be a positive integer.

---

    Code
      ci_prop_wilson_strata(x = as.numeric(rsp), strata = strata, weights = weights +
        pi / 5)
    Condition
      Error in `ci_prop_wilson_strata()`:
      ! The sum of the `weights` argument must be 1

---

    Code
      ci_prop_wilson_strata(x = as.numeric(rsp), strata = strata, weights = weights +
        pi)
    Condition
      Error in `ci_prop_wilson_strata()`:
      ! The `weights` argument must be in the interval `[0, 1]`.

