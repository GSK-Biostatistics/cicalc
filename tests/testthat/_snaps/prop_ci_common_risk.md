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
      ci_rel_risk_cmh_strata(x = results, by = treatment, strata = centre, data = agresti_long)
    Message
      
      -- Mantel-Haenszel Common Relattive Risk Confidence Interval -------------------
      * 55/130 - 47/143
      * Estimate: 1.424
      * Variance: 0.02
      * 95% Confidence Interval:
        (1.0786, 1.8812)

