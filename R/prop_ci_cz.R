

chi_distance <- function(delta, s_x, n_x, s_y, n_y, ref_chi){
  chi_score <- chi_score(delta, s_x, n_x, s_y, n_y)
  ref_chi - chi_score
}

chi_score <- function(delta, s_x, n_x, s_y, n_y){
  p_hat_x <- s_x / n_x
  p_hat_y <- s_y / n_y
  N = n_x + n_y
  var_delta <- variance_mn(s_x = s_x, n_x = n_x,
                           s_y = s_y, n_y = n_y, delta = delta)*(N-1)/N
  chi_delta = ((p_hat_x - p_hat_y - delta) / sqrt(var_delta))^2
  chi_delta
}


score_space <- function(n_x, n_y, delta){
  N <- n_x + n_y
  expand.grid(
    s_y_df = seq(0, n_y),
    s_x_df = seq(0, n_x)
              ) |>
    mutate(score = test_score_mn(s_x = s_x_df, n_x = n_x,
                                 s_y = s_y_df, n_y = n_y, delta = delta))

}

pval_chan <- function(delta, s_x, n_x, s_y, n_y, low){
  score_df <- score_space(n_x = n_x, n_y = n_y, delta = delta)

  obs_score <- score_df |>
    filter(s_x_df == s_x, s_y_df == s_y) |>
    pull(score)
  # Score values that are equal or more exterme
  score_mat <- score_df$score |>
    matrix(ncol = n_y+1, byrow = TRUE)

  if(low){
    # scores less than observed
    eq_ex_mat <- score_mat >= obs_score
  } else {
    # scores equal to or more than observes score
    eq_ex_mat <- score_mat < obs_score
  }

  pval <- optimize(calc_pval, interval = c(max(0,-delta), min(1, 1 - delta)),
           delta = delta, eq_ex_mat = eq_ex_mat, n_x = n_x, n_y = n_y,
           maximum = TRUE)$objective

  pval
}


calc_pval <- function(prob_x, delta, eq_ex_mat, n_x, n_y) {
  prob_y <- prob_x + delta
  sum(eq_ex_mat* dbinom(0:n_x, n_x, prob_x) %o% dbinom(0:n_y, n_y, prob_y), na.rm = TRUE)
}

ci_CZ(9, 3,10, 10)
pval_chan(delta = -0.4821265, s_x = 9 , n_x = 10, s_y = 3, n_y= 10, low= TRUE)


alpha = 0.05
# Domain to look at the
p = 9/10-3/10

ci_prop_diff_cz <- function(s_x , s_y, n_x, n_y){
  alpha = 0.05
  p = s_x/n_x-s_y/n_y
  d_delta_low <- stats::uniroot(chi_distance, interval=c(-1,p),
                                ref_chi = qchisq(1-alpha,1),
                                s_x = s_x, s_y = s_y, n_x = n_x, n_y = n_y,
                                tol=1e-08)$root

  d_delta_high <- stats::uniroot(chi_distance, interval=c(p,0.99999),
                                 ref_chi = qchisq(1-alpha,1),
                                 s_x = s_x, s_y = s_y, n_x = n_x, n_y = n_y,
                                 tol=1e-08)$root

  delta_vec <- seq(max(-d_delta_low-0.3,-0.9999),min(-d_delta_low+0.3,0.9999), by = 1e-3)
  map_dbl(delta_vec, \(d){
    pval_chan(delta = d, s_x = s_x , n_x = n_x, s_y = s_y, n_y= n_y, low= TRUE)
  }
  )
  # foo <- map_dbl(delta_vec, pval_chan,
  #     s_x = s_x , n_x = n_x, s_y = s_y, n_y= n_y, low= TRUE
  #     )
  lower_ci <- -1*stats::uniroot(z_distance, interval=c(max(-d_delta_low-0.3,-0.9999),min(-d_delta_low+0.3,0.9999)),
                    fx=pval_chan,
                    ref_z = (alpha / 2),
                    s_x = s_x , n_x = n_x, s_y = s_y, n_y= n_y, low= TRUE, tol=1e-08)$root


  upper_ci <- -1*stats::uniroot(z_distance, interval=c(max(-d_delta_high-0.3,-0.9999),min(-d_delta_high+0.3,0.9999)),
                    fx=pval_chan,
                    ref_z = (alpha / 2),
                    s_x = s_x , n_x = n_x, s_y = s_y, n_y= n_y, low= FALSE, tol=1e-08)$root

  list(conf.low = lower_ci,
       conf.high = upper_ci)
}

