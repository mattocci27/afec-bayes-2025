library(targets)
library(tarchetypes)
library(tidyverse)
library(stantargets)
library(cmdstanr)
library(furrr)
library(clustermq)


source("R/functions.R")
source("R/mh.R")
source("R/hmc.R")

# parallel computing on local or on the same node
plan(multicore)
options(clustermq.scheduler = "multicore")
cmdstanr::set_cmdstan_path("/opt/cmdstan/cmdstan-2.36.0")


tar_option_set(packages = c(
  "tidyverse",
  "bayesplot",
  "ggrepel",
  "patchwork",
  "janitor",
  "showtext",
  "animation",
  "gganimate",
  "loo"
))

list(
  tar_target(
    dummy_simple,
    generate_dummy_simple(n_sp = 8, sig = 0.2, seed = 500)
  ),
  tar_target(
    dummy_simple_stan,
    generate_dummy_simple_stan(dummy_simple)
  ),

  tar_stan_mcmc(
    simple,
    "stan/logistic.stan",
    data = dummy_simple_stan,
    seed = 123,
    chains = 4,
    parallel_chains = getOption("mc.cores", 4),
    iter_warmup = 1000,
    iter_sampling = 1000,
    refresh = 0
  ),
  tar_target(
    dummy_simple_re,
    add_p(dummy_simple, simple_summary_logistic)
  ),
  tar_target(
    mh_df,
    generate_mh_df()
  ),
  tar_target(
    mh_trace_gif,
    make_mh_trace_gif(mh_df, out = "images/mh_trace.gif"),
    format = "file"
  ),
  tar_target(
    mh_post_gif,
    make_mh_post_gif(mh_df, out = "images/mh_post.gif"),
    format = "file"
  ),
  tar_target(
    leapfrog_list,
    generate_leapfrog_list()
  ),
  tar_target(
    simple_hmc_list,
    generate_simple_hmc_list()
  ),
  tar_target(
    pot_eng_gif,
    make_pot_eng_gif(simple_hmc_list, out = "images/pot_eng.gif"),
    format = "file"
  ),
  tar_target(
    traj_gif,
    make_traj_gif(simple_hmc_list, out = "images/traj.gif"),
    format = "file"
  ),
  tar_target(
    leapfrog_gif,
    make_leapfrog_gif(leapfrog_list, out = "images/leapfrog.gif"),
    format = "file"
  ),
  tar_target(
    hmc_df,
    generate_hmc_df(T = 10000)
  ),
  tar_target(
    hmc_gif,
    make_hmc_gif(
      data = leapfrog_list$cont_dat,
      sim_res = hmc_df,
      out = "images/hmc.gif"),
    format = "file"
  ),
  # tar_quarto(
  #   main,
  #   "main.qmd"
  # ),
  NULL
)
