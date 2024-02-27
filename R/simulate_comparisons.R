#' Simulate comparison Vectors
#'
#' @export

simulate_comparisons <- function(m, u, levels, n1, n2, overlap,
                                 previous_matches = 0){
  field_marker <- unlist(lapply(1:length(levels), function(x){
    rep(x, levels[x])
  }))

  N <- n1 * n2
  ids <- expand.grid(1:n1, 1:n2)
  indicators <- matrix(NA, nrow = N, ncol = length(levels))

  df2matches <- seq_len(overlap)
  df1matches <- df2matches + previous_matches

  Ztrue <- rep(n1 + 1, n2)
  Ztrue[df2matches] <- df1matches

  match_index <- which(ids[,1]  == (ids[,2]+ previous_matches))[seq_len(overlap)]

  m_list <- split(m, field_marker)
  u_list <- split(u, field_marker)

  gamma_match <- sapply(m_list, function(x){
    sample(seq_along(x) - 1, overlap, replace = T, x)
  })

  gamma_nonmatch <- sapply(u_list, function(x){
    sample(seq_along(x) - 1, N - overlap, replace = T, x)
  })

  if(overlap == 0){
    indicators <- gamma_nonmatch
  } else {
  indicators[match_index,] <- gamma_match
  indicators[-match_index,] <- gamma_nonmatch
  }
  Sadinle_indicators <- purrr::map2(data.frame(indicators), levels, ~fs_to_sadinle_2(.x, .y)) %>%
    do.call(cbind, .)

  list(comparisons = Sadinle_indicators,
       n1 = n1,
       n2 = n2,
       nDisagLevs = levels,
       Ztrue = Ztrue)
}

