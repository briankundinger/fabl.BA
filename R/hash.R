#' @export
#'
hash_comparisons <- function(cd, R = NULL, all_patterns = FALSE){

  indicators <- cd[[1]]
  N <- dim(indicators)[1]
  n1 <- cd[[2]]
  n2 <- cd[[3]]

  levels <- cd[[4]]
  fields <- seq_along(cd[[4]])
  field_marker <- sapply(fields, function(x){
    rep(x, cd[[4]][x])
  }) %>%
    unlist(.) %>%
    as.vector(.)

  ids <- expand.grid(1:n1, 1:n2)
  rec1 <- ids[,1]
  rec2 <- ids[,2]

  Lf_vec<- (levels) %>%
    c(0, .) %>%
    cumsum()

  hash_vals <- purrr::imap(cd[[4]], ~hash_field(.x, .y, Lf_vec)) %>%
    unlist()

  hash <- sweep(indicators, 2, hash_vals, "*") %>%
    rowSums() + 1

  if(all_patterns == TRUE){

    unique_patterns <- possible_patterns_sadinle(levels)
    unique_hashed <- sweep(unique_patterns, 2, hash_vals, "*") %>%
      rowSums() + 1
    P <- dim(unique_patterns)[1]
    hash_id <- match(hash, unique_hashed) %>%
      factor(levels = 1:P)

  } else {

    unique_hashed <- unique(hash)
    P <- length(unique_hashed)
    hash_id <- match(hash, unique_hashed) %>%
      factor(levels = 1:P)
    unique_patterns <- indicators[!duplicated(hash_id), ]
  }

  temp <- data.frame(rec1, rec2, hash_id)

  hash_count_list <- temp %>%
    group_by(rec2, hash_id, .drop = F) %>%
    count() %>%
    ungroup() %>%
    group_split(rec2) %>%
    purrr::map(~.x %>%
          select(n) %>%
          pull()
        )

  total_counts <- temp %>%
    group_by(hash_id, .drop = F) %>%
    count() %>%
    pull()

  pattern_lookup <- expand.grid(1:P, 1:n2) %>%
    data.frame() %>%
    setNames(., c("hash_id", "rec2"))

  hash_to_file_1 <- temp %>%
    select(rec1, rec2, hash_id) %>%
    nest_by(rec2, hash_id, .keep = F) %>%
    mutate(hash_id = as.integer(hash_id)) %>%
    rowwise() %>%
    mutate(N = nrow(data))

  hash_to_file_1 <- left_join(x = pattern_lookup,
                              y = hash_to_file_1,
                              by = c("hash_id", "rec2"))

  hash_to_file_1$N[is.na(hash_to_file_1$N)] <- 0

  hash_to_file_1 <- hash_to_file_1 %>%
    group_split(rec2) %>%
    purrr::map(., ~ .x %>%
                 group_split(hash_id)) %>%
    purrr::map(., ~purrr::map(.x, `[[`, "data")) %>%
    purrr::map(., ~purrr::map(., ~ unname(unlist(.x))))

  if(!is.null(R)){
    hash_to_file_1 <- lapply(hash_to_file_1, function(z){
      purrr::map(z, ~sei(.x, R))
    })}




  patterns <- list(ohe = unique_patterns,
                   total_counts = total_counts,
                   hash_count_list = hash_count_list,
                   hash_to_file_1 = hash_to_file_1,
                   field_marker = field_marker,
                   n1 = n1,
                   n2 = n2)
  patterns

}

hash_field <- function(L_f, k, Lf_vec){
  level_seq <- seq_len(L_f)
  as.numeric(level_seq > 0) * 2 ^ ((level_seq) + (as.numeric(k > 1)  * Lf_vec[k]))
}

