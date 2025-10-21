# your log‐like and MCMC from above
like_p <- function(p) {
  # if (p <= 0 || p >= 1) return(-Inf)
  110*log(p) + 90*log(1-p)
}

# mcmc_simple <- function(n_iter = 100, init_p = 0.2) {
#   samples  <- numeric(n_iter)
#   p_curr   <- init_p
#   L_curr   <- like_p(p_curr)
#   for (i in seq_len(n_iter)) {
#     p_prop <- p_curr + sample(c(-0.01, 0.01), 1)
#     L_prop <- like_p(p_prop)
#     if (L_prop > L_curr) {
#       p_curr <- p_prop
#       L_curr <- L_prop
#     }
#     samples[i] <- p_curr
#   }
#   samples
# }

mcmc_simple <- function(n_iter = 100, init_p = 0.2) {
  samples  <- numeric(n_iter)
  p_curr   <- init_p
  L_curr   <- like_p(p_curr)
  for (i in seq_len(n_iter)) {
    p_prop <- p_curr + sample(c(-0.01, 0.01), 1)
    if (p_prop <= 0 || p_prop >= 1) {
      samples[i] <- p_curr
      next
    }
    L_prop <- like_p(p_prop)
    accept_prob <- exp(L_prop - L_curr)  # log-scale ratio
    if (runif(1) < accept_prob) {
      p_curr <- p_prop
      L_curr <- L_prop
    }
    samples[i] <- p_curr
  }
  samples
}

generate_mh_df <- function(n_iter = 200, init_p1 = 0.2, init_p2 = 0.8, seed = 123) {
  set.seed(seed)
  chain1 <- mcmc_simple(n_iter, init_p = init_p1)
  chain2 <- mcmc_simple(n_iter, init_p = init_p2)

  # build a long data frame
  df <- tibble(
    iteration = 1:n_iter,
    `chain 1`  = chain1,
    `chain 2`  = chain2) |>
    pivot_longer(-iteration, names_to = "chain", values_to = "p") |>
    mutate(post = like_p(p))

  df
}


make_mh_trace_gif <- function(df, width = 600, height = 400, out) {
  outdir <- dirname(out)
  fname  <- basename(out)
  p <- ggplot(df, aes(x = iteration, y = p, color = chain)) +
    annotate("rect",
             xmin = 0,      # or 1, whichever your first iter is
             xmax = 100,    # shade all iterations < 100
             ymin = -Inf,   # stretch from bottom …
             ymax =  Inf,   # … to top of panel
             fill = "grey80",
             alpha = 0.4) +
    geom_line(lwd = 2) +
    geom_point(size = 4) +
    labs(
      title = "Iteration: {frame_along}",
      x     = "Iteration",
      y     = "p"
    ) +
    coord_cartesian(xlim = range(df$iteration)) +
    theme_minimal(base_size = 28) +
    theme(
      legend.position = "bottom"
    ) +
    transition_reveal(iteration)

  animate(p, nframes = nrow(df), fps = 40,
        width = width, height = height,
        renderer = gifski_renderer(fname))
  file.rename(fname, out)
  paste(out)

}

make_mh_post_gif <- function(df, width = 600, height = 400, out) {
  outdir <- dirname(out)
  fname  <- basename(out)
  p <- ggplot(df, aes(x = p, y = post, color = chain)) +
    geom_line(lwd = 2) +
    geom_point(size = 4) +
    labs(
      title = "Iteration: {frame_along}",
      x     = "p",
      y     = "Posterior"
    ) +
    theme_minimal(base_size = 28) +
    theme(
      legend.position = "bottom"
    ) +
    transition_reveal(iteration)

  animate(p, nframes = nrow(df), fps = 40,
        width = width, height = height,
        renderer = gifski_renderer(fname))
  file.rename(fname, out)
  paste(out)

}

