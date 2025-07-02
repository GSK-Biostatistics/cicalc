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
    Message
      
      -- Wilson Confidence Interval without continuity correction --------------------
      * 5 responses out of 10
      * Estimate: 0.5
      * 90% Confidence Interval:
        (0.2693, 0.7307)

---

    Code
      ci_prop_wilson(x_true)
    Message
      
      -- Wilson Confidence Interval without continuity correction --------------------
      * 32 responses out of 32
      * Estimate: 1
      * 95% Confidence Interval:
        (0.8928, 1)

---

    Code
      ci_prop_wilson(x_false)
    Message
      
      -- Wilson Confidence Interval without continuity correction --------------------
      * 0 responses out of 32
      * Estimate: 0
      * 95% Confidence Interval:
        (0, 0.1072)

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
    Message
      
      -- Wald Confidence Interval with Continuity Correction -------------------------
      * 5 responses out of 10
      * Estimate: 0.5
      * 95% Confidence Interval:
        (0.1401, 0.8599)

---

    Code
      ci_prop_wald(x_true)
    Message
      
      -- Wald Confidence Interval without Continuity Correction ----------------------
      * 32 responses out of 32
      * Estimate: 1
      * 95% Confidence Interval:
        (1, 1)

---

    Code
      ci_prop_wald(x_false)
    Message
      
      -- Wald Confidence Interval without Continuity Correction ----------------------
      * 0 responses out of 32
      * Estimate: 0
      * 95% Confidence Interval:
        (0, 0)

---

    Code
      clopper_pearson_dbl <- ci_prop_clopper_pearson(x_dbl, conf.level = 0.9)

---

    Code
      clopper_pearson_lgl <- ci_prop_clopper_pearson(x_lgl, conf.level = 0.9)

---

    Code
      ci_prop_clopper_pearson(x_rsp, conf.level = 0.95)
    Message
      
      -- Clopper-Pearson Confidence Interval -----------------------------------------
      * 5 responses out of 10
      * Estimate: 0.5
      * 95% Confidence Interval:
        (0.1871, 0.8129)

---

    Code
      ci_prop_clopper_pearson(x_true)
    Message
      
      -- Clopper-Pearson Confidence Interval -----------------------------------------
      * 32 responses out of 32
      * Estimate: 1
      * 95% Confidence Interval:
        (0.8911, 1)

---

    Code
      ci_prop_clopper_pearson(x_false)
    Message
      
      -- Clopper-Pearson Confidence Interval -----------------------------------------
      * 0 responses out of 32
      * Estimate: 0
      * 95% Confidence Interval:
        (0, 0.1089)

---

    Code
      agresti_coull_dbl <- ci_prop_agresti_coull(x_dbl, conf.level = 0.9)

---

    Code
      agresti_coull_lgl <- ci_prop_agresti_coull(x_lgl, conf.level = 0.9)

---

    Code
      ci_prop_agresti_coull(x_rsp, conf.level = 0.95)
    Message
      
      -- Agresti-Coull Confidence Interval -------------------------------------------
      * 5 responses out of 10
      * Estimate: 0.5
      * 95% Confidence Interval:
        (0.2366, 0.7634)

---

    Code
      ci_prop_agresti_coull(x_true)
    Message
      
      -- Agresti-Coull Confidence Interval -------------------------------------------
      * 32 responses out of 32
      * Estimate: 1
      * 95% Confidence Interval:
        (0.8727, 1)

---

    Code
      ci_prop_agresti_coull(x_false)
    Message
      
      -- Agresti-Coull Confidence Interval -------------------------------------------
      * 0 responses out of 32
      * Estimate: 0
      * 95% Confidence Interval:
        (0, 0.1273)

---

    Code
      jeffreys_dbl <- ci_prop_jeffreys(x_dbl, conf.level = 0.9)

---

    Code
      jeffreys_lgl <- ci_prop_jeffreys(x_lgl, conf.level = 0.9)

---

    Code
      ci_prop_jeffreys(x_rsp, conf.level = 0.95)
    Message
      
      -- Jeffreys Interval -----------------------------------------------------------
      * 5 responses out of 10
      * Estimate: 0.5
      * 95% Confidence Interval:
        (0.2235, 0.7765)

---

    Code
      ci_prop_jeffreys(x_true)
    Message
      
      -- Jeffreys Interval -----------------------------------------------------------
      * 32 responses out of 32
      * Estimate: 1
      * 95% Confidence Interval:
        (0.9251, 1)

---

    Code
      ci_prop_jeffreys(x_false)
    Message
      
      -- Jeffreys Interval -----------------------------------------------------------
      * 0 responses out of 32
      * Estimate: 0
      * 95% Confidence Interval:
        (0, 0.0749)

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
    Message
      
      -- Stratified Wilson Confidence Interval without continuity correction ---------
      * 50 responses out of 80
      * Weights: 0.048, 0.095, 0.143, 0.19, 0.238, 0.286
      * Estimate: 0.625
      * 95% Confidence Interval:
        (0.4867, 0.7186)

---

    Code
      ci_prop_wilson_strata(x = rsp, strata = strata, weights = weights, correct = TRUE)
    Message
      
      -- Stratified Wilson Confidence Interval with continuity correction ------------
      * 50 responses out of 80
      * Weights: 0.048, 0.095, 0.143, 0.19, 0.238, 0.286
      * Estimate: 0.625
      * 95% Confidence Interval:
        (0.4483, 0.7531)

---

    Code
      ci_prop_wilson_strata(x = as.numeric(rsp), strata = strata, weights = weights)
    Message
      
      -- Stratified Wilson Confidence Interval without continuity correction ---------
      * 50 responses out of 80
      * Weights: 0.048, 0.095, 0.143, 0.19, 0.238, 0.286
      * Estimate: 0.625
      * 95% Confidence Interval:
        (0.4867, 0.7186)

---

    Code
      ci_prop_wilson_strata(x = as.numeric(rsp), strata = strata)
    Message
      
      -- Stratified Wilson Confidence Interval without continuity correction ---------
      * 50 responses out of 80
      * Weights: a.x = 0.211, b.x = 0.189, a.y = 0.118, b.y = 0.154, a.z = 0.174, b.z
      = 0.153
      * Estimate: 0.625
      * 95% Confidence Interval:
        (0.5242, 0.7269)

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

