library(RecordLinkage)
library(dplyr)
library(stringr)
library(fabl.BA)
library(purrr)
library(readr)
library(BRL)

# taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
# i = taskID
i <- 1 # Choose an integer 1-300 to run a different simulation
set.seed(41)
files <- list.files(path = "data/sadinle_sim_data/", full.names = T)

m_prior = 1
u_prior = 1
alpha = 1
beta = 1
S = 1000
burn = 100
show_progress = F
R = NULL
all_patterns = TRUE
resolve = T


overlap_vals <- c(50, 250, 450)


fabl_samps <- matrix(NA, nrow = 3, ncol = 6)
fabl_partial_samps <- matrix(NA, nrow = 3, ncol = 6)

sad_samps <- matrix(NA, nrow = 3, ncol = 6)
sad_partial_samps <- matrix(NA, nrow = 3, ncol = 6)

for(j in seq_along(overlap_vals)){

  overlap <- overlap_vals[j]

  records <- read_csv(files[i], col_types = cols())
  records$file <- rep(2:1, length.out = dim(records)[1])

  records <- records %>%
    janitor::clean_names() %>%
    mutate(rec_id = as.numeric(str_extract(rec_id, "\\d{3}")) + 1)

  n1 <- 500
  n2 <- 500
  #overlap <- n2/2

  # Ztrue <- n1 + 1:n2
  # Ztrue[1:overlap] <- 1:overlap

  Ztrue <- rep(0, n2)
  Ztrue[1:overlap] <- 1:overlap

  file1 <- records %>%
    filter(file ==1,
           rec_id <= n1) %>%
    select(-rec_id) %>%
    as.matrix(.) %>%
    data.frame(.) %>%
    mutate(occup = as.numeric(occup))

  file2 <- records %>%
    filter(file == 2,
           rec_id %in% c(1:overlap, (n1 +1):(1000 - overlap))) %>%
    select(-rec_id) %>%
    as.matrix() %>%
    data.frame(.) %>%
    mutate(occup = as.numeric(occup))


  cd <- compare_records(file1, file2, c(2, 3, 5, 6),
                        types = c("lv", "lv", "bi", "bi"))
                        #breaks = c(0, .25))
  cd[[1]] <- apply(cd[[1]], 2, as.numeric)


  hash <- hash_comparisons(cd)



  # fabl
  ptm <- proc.time()
  Zchain <- gibbs_efficient(hash)
  elapsed <- proc.time() - ptm
  result <- estimate_links(Zchain[[1]][, 101:1000], n1, 1, 1, 2, Inf)
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  fabl_samps[j, ] <- c(eval, NA, elapsed[3], overlap)

  result <- estimate_links(Zchain[[1]][,101:1000], n1, 1, 1, 2, .1)
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  RR <- sum(result$Z_hat == -1)/n2
  eval[1] <- sum(result$Z_hat == Ztrue & Ztrue == 0) / (sum(result$Z_hat == 0))
  fabl_partial_samps[j, ] <- c(eval, RR, elapsed[3], overlap)



  #Sadinle 2017 Method
  ptm <- proc.time()
  Zchain <- BRL::bipartiteGibbs(cd)[[1]]
  elapsed <- proc.time() - ptm
  result <- estimate_links(Zchain[, 101:1000], n1, 1, 1, 2, Inf)
  result$Z_hat[result$Z_hat > n1] <- 0
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  sad_samps[j, ] <- c(eval, NA, elapsed[3], overlap)

  result <- estimate_links(Zchain[, 101:1000], n1, 1, 1, 2, .1)
  RR <- sum(result$Z_hat == -1)/n2
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  eval[1] <- sum(result$Z_hat == Ztrue & Ztrue == 0) / (sum(result$Z_hat == 0))
  sad_partial_samps[j, ] <- c(eval, RR, elapsed[3], overlap)

  #  print(i)
  #}
}


fabl_samps <- data.frame(fabl_samps, "fabl") %>%
  unname() %>%
  data.frame()
fabl_partial_samps <- data.frame(fabl_partial_samps, "fabl_partial") %>%
  unname() %>%
  data.frame()
sad_samps <- data.frame(sad_samps, "BRL") %>%
  unname() %>%
  data.frame()
sad_partial_samps <- data.frame(sad_partial_samps, "BRL_partial") %>%
  unname() %>%
  data.frame()

result_df <- rbind(fabl_samps, fabl_partial_samps,
                   sad_samps, sad_partial_samps)

if(i < 100){
  error <- "One Error"
} else if (i < 200) {
  error <- "Two Errors"
} else {
  error <- "Three Errors"
}
names(result_df) <- c("recall", "precision", "f-measure", "RR",
                      "time", "overlap", "method")
result_df$error <- error

saveRDS(result_df, file = paste0("out/sadinle_sim/sim_",
                                      str_pad(i, 3, pad = "0")))
