# Check print

    Code
      ci_prop_diff_mh_strata(x = results, by = treatment, strata = centre, data = agresti_long)
    Message
      
      -- Mantel-Haenszel Risk Difference Confidence Interval, Sato Variance ----------
      * 55/130 - 47/143
      * Estimate: 0.13
      * Variance: 0.003
      * 95% Confidence Interval:
        (0.0313, 0.2284)

---

    Code
      ci_prop_diff_mh_strata(x = results, by = treatment, strata = centre, sato_var = FALSE,
        data = agresti_long)
    Message
      
      -- Mantel-Haenszel Risk Difference Confidence Interval, Independent Binomial ---
      * 55/130 - 47/143
      * Estimate: 0.13
      * Variance: 0.002
      * 95% Confidence Interval:
        (0.0348, 0.2249)

---

    Code
      ci_rel_risk_cmh_strata(x = results, by = treatment, strata = centre, data = agresti_long)
    Message
      
      -- Mantel-Haenszel Common Relattive Risk Confidence Interval -------------------
      * 55/130 - 47/143
      * Estimate: 1.424
      * Variance: 0.02
      * 95% Confidence Interval:
        (1.0786, 1.8812)

# check the ci_prop_diff_nc_strata() function works

    Code
      ci_prop_diff_nc_strata(x = rsp, by = grp, strata = strata, weights_method = "wilson",
        correct = FALSE)
    Message
      
      -- Stratified Newcombe Confidence Interval without continuity correction, Wilson
      * 50 responses out of 80
      * Weights: a.x = 0.211, b.x = 0.189, a.y = 0.118, b.y = 0.154, a.z = 0.174, b.z
      = 0.153
      * Estimate: 0.259
      * 95% Confidence Interval:
        (0.0392, 0.4502)

---

    Code
      ci_prop_diff_nc_strata(x = rsp, by = grp, strata = strata, weights_method = "wilson",
        correct = TRUE)
    Message
      
      -- Stratified Newcombe Confidence Interval with continuity correction, Wilson --
      * 50 responses out of 80
      * Weights: a.x = 0.211, b.x = 0.189, a.y = 0.118, b.y = 0.154, a.z = 0.174, b.z
      = 0.153
      * Estimate: 0.259
      * 95% Confidence Interval:
        (0.0398, 0.4366)

---

    Code
      ci_prop_diff_nc_strata(x = rsp, by = grp, strata = strata, weights_method = "cmh",
        correct = FALSE)
    Message
      
      -- Stratified Newcombe Confidence Interval without continuity correction, CMH --
      * 50 responses out of 80
      * Weights: a.x = 0.195, b.x = 0.163, a.y = 0.135, b.y = 0.138, a.z = 0.202, b.z
      = 0.167
      * Estimate: 0.254
      * 95% Confidence Interval:
        (0.0347, 0.4454)

---

    Code
      ci_prop_diff_nc_strata(x = rsp, by = grp, strata = strata, weights_method = "cmh",
        correct = TRUE)
    Message
      
      -- Stratified Newcombe Confidence Interval with continuity correction, CMH -----
      * 50 responses out of 80
      * Weights: a.x = 0.195, b.x = 0.163, a.y = 0.135, b.y = 0.138, a.z = 0.202, b.z
      = 0.167
      * Estimate: 0.254
      * 95% Confidence Interval:
        (0.0362, 0.4314)

---

    Code
      ci_prop_diff_nc_strata(x = as.numeric(rsp), by = grp, strata = strata,
      weights_method = "wilson")
    Message
      
      -- Stratified Newcombe Confidence Interval without continuity correction, Wilson
      * 50 responses out of 80
      * Weights: a.x = 0.211, b.x = 0.189, a.y = 0.118, b.y = 0.154, a.z = 0.174, b.z
      = 0.153
      * Estimate: 0
      * 95% Confidence Interval:
        (0, 0)

---

    Code
      ci_prop_diff_nc_strata(x = as.numeric(rsp), by = grp, strata = strata,
      weights_method = "cmh")
    Message
      
      -- Stratified Newcombe Confidence Interval without continuity correction, CMH --
      * 50 responses out of 80
      * Weights: a.x = 0.195, b.x = 0.163, a.y = 0.135, b.y = 0.138, a.z = 0.202, b.z
      = 0.167
      * Estimate: 0
      * 95% Confidence Interval:
        (0, 0)

---

    Code
      ci_prop_diff_nc_strata(x = as.numeric(rsp), by = grp, strata = strata)
    Message
      
      -- Stratified Newcombe Confidence Interval without continuity correction, Wilson
      * 50 responses out of 80
      * Weights: a.x = 0.211, b.x = 0.189, a.y = 0.118, b.y = 0.154, a.z = 0.174, b.z
      = 0.153
      * Estimate: 0
      * 95% Confidence Interval:
        (0, 0)

---

    Code
      ci_prop_diff_nc_strata(x = rep_len(TRUE, length(rsp)), by = grp, strata = strata,
      weights_method = "wilson")
    Condition
      Error in `ci_prop_wilson_strata()`:
      ! All values in `x` argument are either `TRUE` or `FALSE` and CI is not estimable.

---

    Code
      ci_prop_diff_nc_strata(x = rep_len(FALSE, length(rsp)), by = grp, strata = strata,
      weights_method = "cmh")
    Condition
      Error:
      ! All values in `x` argument are either `TRUE` or `FALSE` and CI is not estimable.

---

    Code
      ci_prop_diff_nc_strata(x = rsp, by = grp, strata = strata, weights_method = "wilson",
        correct = FALSE)
    Condition
      Warning:
      Less than 5 observations in some strata.
      Error in `ci_prop_diff_nc_strata()`:
      ! Arguments `unique(strata)` and `weights` must be the same length.

