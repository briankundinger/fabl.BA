library(fabl.BA)
library(glue)
library(BRL)

#k = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
#k <- 1 # Choose an integer 1-8 to run a different simulation
n1_vals <- c(500, 1000, 2000, 3000, 4000, 5000, 6000, 7000)
n2 <- 500

for (k in 1:length(n1_vals)) {
  n1 <- n1_vals[k]

  total_overlap <- n2 / 2
  S = 1000
  burn = S * .1

  Z_true <- rep(0, n2)
  Z_true[1:total_overlap] <- 1:total_overlap

  show_progress <- T
  fast = F
  R <- NULL
  all_patterns <- T

  m <- c(.05, .95, .05, .95, .05, .95, .05, .95, .05, .95)

  u <- c(.99, .01, .99, .01,
         1 - 1 / 30, 1 / 30, 1 - 1 / 12, 1 / 12, 1 - 1 / 15, 1 / 15)

  levels <- c(2, 2, 2, 2, 2)




  cd <- simulate_comparisons(m, u, levels, n1, n2, total_overlap)
  hash <- hash_comparisons(cd, all_patterns = T)


  ptm <- proc.time()
  out <- BRL::bipartiteGibbs(cd, nIter = S)
  seconds <- (proc.time() - ptm)[3]
  brl_df <- data.frame(
    n1 = n1,
    time = seconds,
    iterations = S,
    method = "BRL"
  )


  ptm <- proc.time()
  out <- gibbs_efficient(hash, S = S, burn = burn)
  seconds <- (proc.time() - ptm)[3]
  result <- estimate_links(out$Z, n1)
  fabl_df <- data.frame(
    n1 = n1,
    time = seconds,
    iterations = S,
    method = "fabl"
  )

  df <- rbind(brl_df, fabl_df)
  saveRDS(df,
          glue("out/speed_big/n_{stringr::str_pad(k, width = 2, pad = 0)}"))
}
