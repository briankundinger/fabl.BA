library(tidyverse)

files <- list.files("out/speed_big/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .)

results %>%
  ggplot() +
  aes(x = n1, color = method, y = time) +
  geom_line(linewidth = .8) +
  theme_bw() +
  labs(y = "Inference Time (Seconds)",
       x = expression(paste(n[1], ",", n[2])))
