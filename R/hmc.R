# theta <- seq(0.01, 3, length = 200)

h <- function(theta, shape, rate) {
  #-dgamma(theta, shape = shape, rate = rate, log = T)
  rate * theta - (shape - 1) * log(theta)
}

dh_dtheta <- function(theta, shape, rate) {
  rate - (shape - 1) /theta
}

hamiltonian <- function(p, theta, shape, rate) {
  h(theta, shape, rate) + 0.5 * p^2
}

leapfrog_nexthalf_p <- function(p, theta, shape, rate, eps = 0.01) {
  p - 0.5 * eps * dh_dtheta(theta, shape, rate)
}

leapfrog_next_theta <- function(p, theta, eps = 0.01) {
  theta + eps * p
}

move_one_step <- function(theta, shape, rate, p, eps = 0.01, L = 100, stlide = 1) {
  res <- c(1, p, theta, hamiltonian(p, theta, shape, rate))
  res2 <- NULL
  for (i in 1:L) {
    p <- leapfrog_nexthalf_p(p, theta, shape, rate, eps)
    theta <- leapfrog_next_theta(p, theta, eps)
    p <- leapfrog_nexthalf_p(p, theta, shape, rate, eps)
    res <- c(1, p, theta, hamiltonian(p, theta, shape, rate))
    res2 <- rbind(res2, res)
  }
  res3 <- res2 %>%
    as_tibble %>%
    rename(temp = V1,
           p = V2,
           theta = V3,
           hamiltonian = V4)
  res3 %>%
    filter(row_number() %% stlide == 0)
}


pot_eng <- function(theta, shape, rate) {
  rate * theta - (shape - 1) * log(theta)
}

p_eng <- function(H, h){
  sqrt(2 * (H - h))
}

h <- function(theta, shape, rate) {
  #-dgamma(theta, shape = shape, rate = rate, log = T)
  rate * theta - (shape - 1) * log(theta)
}

# what's this?
dh_dtheta <- function(theta, shape, rate) {
  rate - (shape - 1) /theta
}

hamiltonian <- function(p, theta, shape, rate) {
  h(theta, shape, rate) + 0.5 * p^2
}

leapfrog_nexthalf_p <- function(p, theta, shape, rate, eps = 0.01) {
  p - 0.5 * eps * dh_dtheta(theta, shape, rate)
}

leapfrog_next_theta <- function(p, theta, eps = 0.01) {
  theta + eps * p
}

generate_contour_data <- function(shape = 11, rate = 13) {
  xx <- seq(-5, 5, length = 200)
  yy <- seq(0.01, 3, length = 200)
  xy <- expand.grid(xx, yy)

  zz <- hamiltonian(p = xy$Var1, theta = xy$Var2, shape  = shape, rate = rate)

  tibble(p = xy$Var1,
             theta = xy$Var2,
             hamiltonian = zz)
}


# cont_dat <- generate_contour_data()
# shape <- 11
# rate <- 13
# theta <- 2.5
# p <- 0
# eps <- 0.05
# L <- 96
# T <- 15
# scale_p <- 1
# prev_p <- 0
# seed <-  123

# leapfrog_list <- generate_leapfrog_list()

generate_leapfrog_list <- function(theta = 2.5, shape = 11, rate = 13, p = 0, eps = 0.01, T = 15, L = 100, stlide = 1, seed = 123) {
  set.seed(seed)

  step <- NULL
  step2 <- NULL
  prev_p <- 0
  scale_p <- 1

  for (tau in 1:T) {
    p <- rnorm(1, 0, scale_p)
    step <- rbind(step, c(2, p, prev_p, 0, tau))
    one_step <- move_one_step(theta, shape, rate,
                              p, eps = eps, L = L,
                              stlide = 12) %>%
      mutate(gr = tau) %>%
      as.matrix

    one_step2 <- one_step %>%
      as_tibble %>%
      slice(c(1, nrow(.))) %>%
      mutate(gr3 = c(tau, tau+1))

    theta <- one_step[nrow(one_step), "theta"] %>% as.numeric
    prev_p <- one_step[nrow(one_step), "p"] %>% as.numeric
    step <- rbind(step, one_step)
    step2 <- rbind(step2, one_step2)
  }

  step <- as_tibble(step) %>%
    filter(temp != 2) %>%
    mutate(gr = as.factor(gr))

  step2 <- step2 %>%
    as_tibble %>%
    mutate(gr3 = as.factor(gr3))

  list(
    cont_dat = generate_contour_data(),
    step = step,
    step2 = step2
  )
}

generate_simple_hmc_list <- function(theta = 0.1, shape = 11, rate = 13, p = 0, eps = 0.01, L = 200) {
  # theta <- 2.5
# theta <- 0.1
# p <- 0
# eps <- 0.01
# L <- 200
# shape <- 11
# rate <- 13

  # dh_dtheta(theta, shape, rate)
  # hamiltonian(p, theta, shape, rate)
  # leapfrog_nexthalf_p(p, theta, shape, rate, eps)
  # leapfrog_next_theta(p, theta, eps)
  cont_df <- generate_contour_data(shape, rate)

  new_hm <- move_one_step(theta = theta, shape = shape, rate = rate, p = p, eps = eps, L = L)

  hmc_df <- tibble(h = pot_eng(new_hm$theta, shape = shape, rate = rate),
                       theta = new_hm$theta) %>%
    mutate(p = p_eng(max(h), h)) %>%
    mutate(hamiltonian = h + 0.5 * p^2) %>%
    mutate(theta_diff = theta - c(0, theta)[-201]) %>%
    mutate(p = ifelse(theta_diff > 0, p, -p))

  list(
    new_hm = new_hm,
    hmc_df = hmc_df,
    cont_df = cont_df
  )
}


make_pot_eng_gif <- function(simple_hmc_list, n_max = 200, width = 600, height = 400, out) {
  outdir <- dirname(out)
  fname  <- basename(out)
  df <- simple_hmc_list$hmc_df

  anim_df <- df %>%
  slice(1:n_max) %>%
  mutate(frame = row_number())

  p <- ggplot(anim_df, aes(x = theta, y = h)) +
    geom_line(data = df, aes(x = theta, y = h)) +
    geom_point(color = "blue", size = 4) +
    geom_segment(
      aes(
        x    = theta,
        xend = ifelse(p > 0,
                      theta + exp(p)/100,
                      theta - exp(-p)/100),
        y    = h,
        yend = h
      ),
      arrow = arrow(length = unit(0.05, "npc")),
      color = "blue"
    ) +
    coord_cartesian(xlim = c(-0.2, 3.5)) +
    theme_bw(base_size = 28) +
    # labs(title = "Potential energy (h) — fr") +
    transition_reveal(frame)

  animate(p, nframes = nrow(df), fps = 40,
        width = width, height = height,
        renderer = gifski_renderer(fname))
  file.rename(fname, out)
  paste(out)

}

make_traj_gif <- function(simple_hmc_list, n_max = 200, width = 600, height = 400, out) {
  outdir <- dirname(out)
  fname  <- basename(out)
  df <- simple_hmc_list$new_hm
  anim_df <- df %>%
    slice(1:n_max) %>%
    mutate(frame = row_number())

  p <- ggplot(simple_hmc_list$cont_df, aes(x = theta, y = p)) +
    geom_contour(aes(z = hamiltonian),
                 bins = 50, lwd = 0.2) +
    geom_point(data = anim_df,
               aes(x = theta, y = p),
               color = "red", size = 4) +
    coord_cartesian(xlim = c(-0.2, 3.5),
                    ylim = c(min(df$p), max(df$p))) +
    theme_bw(base_size = 28) +
    ylab("r") +
    # labs(title = "Phase space (p vs θ) — frame {current_frame}") +
    transition_reveal(frame)

  animate(p, nframes = nrow(df), fps = 40,
        width = width, height = height,
        renderer = gifski_renderer(fname))
  file.rename(fname, out)
  paste(out)

}
generate_leapfrog_frame <- function(n, data, step, step2) {
    step_n <- step %>%
      filter(row_number() <= n)
    step2_n <- step2 %>%
      filter(gr3 %in% step_n$gr)

    plt1 <- ggplot(step_n) +
      geom_point(data = step_n, aes(x = theta, y = p), size = 4) +
      geom_contour(data = data,
                   aes(x = theta, y = p, z = hamiltonian),
                   bins = 50,
                   lwd = 0.2) +
      geom_line(data = step2_n,
                aes(x = theta, y = p, group = gr3),
                col = "red") +
      geom_path(aes(x = theta, y = p, group = gr), lty = 2) +
      ylab("r") +
      coord_cartesian(xlim = c(-0.2, 3.5),
                      ylim = c(-5, 5)) +
      theme_bw(base_size = 28)
    print(plt1)
 }


make_leapfrog_gif <- function(leapfrog_list, out) {
  outdir <- dirname(out)
  fname  <- basename(out)

  saveGIF(
    lapply(seq_len(nrow(leapfrog_list$step)),
           function(i) generate_leapfrog_frame(i, leapfrog_list$cont_dat, leapfrog_list$step, leapfrog_list$step2)),
    interval   = 0.05,
    ani.width  = 600,
    ani.height = 400,
    movie.name = fname
  )

  file.rename(fname, out)
  paste(out)
}

# make_leapfrog_gif(leapfrog_list, out = "images/leapfrog.gif")

generate_hmc_df <- function(theta = 2.5, shape = 11, rate = 13, p = 0, eps = 0.01, T = 15, L = 100, stlide = 1, seed = 123) {
  set.seed(seed)
  scale_p <- 1

  # initial param
  p <- rnorm(1, 0, scale_p)
  prev_hamiltonian <- hamiltonian(p, theta, shape, rate)
  sim_res <- c(p, theta, prev_hamiltonian, TRUE)

  for (t in 1:T){
    prev_p <- p
    prev_theta <- theta
    prev_hamiltonian <- hamiltonian(p, theta, shape, rate)
      for (i in 1:L) {
        p <- leapfrog_nexthalf_p(p, theta, shape, rate, eps = eps)
        theta <- leapfrog_next_theta(p, theta, eps = eps)
        p <- leapfrog_nexthalf_p(p, theta, shape, rate, eps = eps)
      }
    H <- hamiltonian(p, theta, shape, rate)
    r <- exp(prev_hamiltonian - H)
    if (r > 1) {
      sim_res <- rbind(sim_res, c(p, theta, prev_hamiltonian, TRUE))
    } else if (r > 0 & runif(1) < r) {
      sim_res <- rbind(sim_res, c(p, theta, prev_hamiltonian, TRUE))
    } else {
      sim_res <- rbind(sim_res, c(p, theta, prev_hamiltonian, FALSE))
      theta <- prev_theta
    }
    p <- rnorm(1, 0, scale_p)
  }

  colnames(sim_res) <- c("p", "theta", "hamiltonian", "accept")
  rownames(sim_res) <- NULL

  as.data.frame(sim_res) |> as_tibble()
}


generate_hmc_frame <- function(n_iter, n_burn, data, sim_res) {
  sim_res_n <- sim_res %>%
    filter(row_number() <= n_iter) %>%
    mutate(burn = ifelse(row_number() <= n_burn,
                         "discard",
                         "keep"))
  plt1 <- ggplot(sim_res_n) +
    geom_contour(data = data,
                 aes(x = theta, y = p, z = hamiltonian),
                 bins = 50,
                 lwd = 0.2) +
    geom_path(data = sim_res_n,
              aes(x = theta, y = p),
              col = "#2E7D32",
              lwd = 0.2) +
    geom_point(data = sim_res_n, aes(x = theta, y = p, shape = burn), size = 4) +
    scale_shape_manual(values = c(1, 16)) +
    ylab("r") +
    coord_cartesian(xlim = c(-0.2, 3.5),
                    ylim = c(-5, 5)) +
    theme_bw(base_size = 28) +
    theme(legend.position = "none")
  print(plt1)
}


make_hmc_gif <- function(n_iter = 200, n_burn = 50, data, sim_res, out) {
  outdir <- dirname(out)
  fname  <- basename(out)

  saveGIF(
    lapply(seq_len(n_iter),
           function(i) generate_hmc_frame(i, n_burn, data, sim_res)),
    interval   = 0.05,
    ani.width  = 600,
    ani.height = 400,
    movie.name = fname
  )

  file.rename(fname, out)
  paste(out)
}
