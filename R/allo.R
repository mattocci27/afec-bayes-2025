generate_allo_data <- function() {
  set.seed(12345)
  n_sp <- 10
  n_rep <- 20
  wd <- rnorm(n_sp, 0, 1)
  gamma0 <- 0.6
  gamma1 <- 0.1
  sigma_y <- 0.1

  b1_hat <- gamma1 * wd + gamma0
  b1 <- rnorm(n_sp, b1_hat, 0.01)
  log_b0 <- rnorm(n_sp, 0.55, 0.05)

  # ---- simulate ----
  allo_df <- tibble(
    sp     = factor(rep(paste0("sp", 1:n_sp), each = n_rep)),
    wd  = rep(wd, each = n_rep),
    # now log_xx ~ Normal(mean log‐dbh, sd
    log_xx = rnorm(n_sp * n_rep, mean = log(40), sd = 0.5)) |>
    mutate(
      # add observation‐level noise on log‐height
      log_y = rnorm(
        n(),
        log_b0[as.integer(sp)] + b1[as.integer(sp)] * log_xx,
        sigma_y),
      dbh = exp(log_xx),
      h = exp(log_y)) |>
    select(sp, wd, dbh, h)

}


generate_allo_list <- function(allo_df) {
  allo_vslope_list <- list(
    y = log(allo_df$h),
    x = cbind(1, log(allo_df$dbh)),
    jj = as.integer(allo_df$sp),
    N = nrow(allo_df),
    K = 2, # number of predictors (intercept + slope)
    J = allo_df$sp |> unique() |> length()
  )
  allo_vslope_list$L <- 2 # number of group-level predictors (intercept and wd)
  allo_vslope_list$u <- matrix(1, nrow = allo_vslope_list$L, ncol = allo_vslope_list$J)

  allo_vslope_list
}
