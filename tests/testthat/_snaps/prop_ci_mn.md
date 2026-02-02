# ci_prop_diff_mn matches the values in the paper

    Code
      ci_prop_diff_mn(response, treat, data = df)
    Message
      
      -- Miettinen-Nurminen Confidence Interval --------------------------------------
      * 10/10 - 0/20
      * Estimate: 1
      * 95% Confidence Interval:
        (0.7156, 1)

# More than 2 levels in strata

    Code
      output
    Message
      
      -- Stratified Summary Score Miettinen-Nurminen Confidence Interval -------------
      * 60/100 - 62/100
      * Weights: 1.1.1 = 0.102, 2.1.1 = 0.147, 1.2.1 = 0.148, 2.2.1 = 0.111, 1.1.2 =
      0.137, 2.1.2 = 0.124, 1.2.2 = 0.132, 2.2.2 = 0.098
      * Estimate: -0.017
      * 95% Confidence Interval:
        (-0.1448, 0.1105)
      
      -- Delta 
      * At -0.1 the statistic is 1.051 and the p-value is 0.1467

# Test when strata group is missing events

    Code
      ci_prop_diff_mn_strata(x = response, by = trt, strata = c(region, sex, gender),
      method = c("summary score"), conf.level = 0.95, delta = -0.1, data = exData2)
    Message
      At least one stratum has a 0 response
      
      -- Stratified Summary Score Miettinen-Nurminen Confidence Interval -------------
      * 60/100 - 62/100
      * Weights: 1.1.1 = 0.025, 2.1.1 = 0.082, 1.2.1 = 0.043, 2.2.1 = 0.176, 1.1.2 =
      0.025, 2.1.2 = 0.2, 1.2.2 = 0.068, 2.2.2 = 0.382
      * Estimate: -0.013
      * 95% Confidence Interval:
        (-0.1382, 0.113)
      
      -- Delta 
      * At -0.1 the statistic is 1.228 and the p-value is 0.1098

---

    Code
      ci_prop_diff_mn_strata(x = response, by = trt, strata = c(region, sex, gender),
      method = c("score"), conf.level = 0.95, delta = -0.1, data = exData2)
    Message
      At least one stratum has a 0 response
      
      -- Stratified Score Miettinen-Nurminen Confidence Interval ---------------------
      * 60/100 - 62/100
      * Weights: 1.1.1 = 0.5, 2.1.1 = 3.938, 1.2.1 = 1.5, 2.2.1 = 9.263, 1.1.2 = 0.5,
      2.1.2 = 9.951, 1.2.2 = 2.727, 2.2.2 = 21
      * Estimate: -0.015
      * 95% Confidence Interval:
        (-0.1499, 0.1224)
      
      -- Delta 
      * At -0.1 the statistic is 1.228 and the p-value is 0.1098

