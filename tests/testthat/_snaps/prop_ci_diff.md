# Test Print

    Code
      ci_prop_diff_wald(response, treat)
    Message
      
      -- Wald Confidence Interval without Continuity Correction ----------------------
      * 56/70 - 48/80
      * Estimate: 0.2
      * 95% Confidence Interval:
        (0.0575, 0.3425)

---

    Code
      ci_prop_diff_wald(response, treat, correct = TRUE)
    Message
      
      -- Wald Confidence Interval with Continuity Correction -------------------------
      * 56/70 - 48/80
      * Estimate: 0.2
      * 95% Confidence Interval:
        (0.0441, 0.3559)

---

    Code
      ci_prop_diff_haldane(response, treat)
    Message
      
      -- Haldane Confidence Interval -------------------------------------------------
      * 56/70 - 48/80
      * Estimate: 0.194
      * 95% Confidence Interval:
        (0.0535, 0.3351)

---

    Code
      ci_prop_diff_jp(response, treat)
    Message
      
      -- Jeffreys-Perks Confidence Interval ------------------------------------------
      * 56/70 - 48/80
      * Estimate: 0.194
      * 95% Confidence Interval:
        (0.0531, 0.3355)

---

    Code
      ci_prop_diff_mee(response, treat)
    Message
      
      -- Mee Confidence Interval -----------------------------------------------------
      * 56/70 - 48/80
      * Estimate: 0.2
      * 95% Confidence Interval:
        (0.0533, 0.3377)

