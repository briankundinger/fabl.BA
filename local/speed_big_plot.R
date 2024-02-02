library(tidyverse)

files <- list.files("out/speed_big/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .)
  #filter(method %in% c("BRL", "fabl", "vabl")) %>%
  #mutate(method = factor(method, c("BRL", "fabl", "vabl")))

results %>%
  ggplot() +
  aes(x = n1, color = method, y = time) +
  geom_line(linewidth = .8) +
  theme_bw() +
  labs(y = "Inference Time (Seconds)",
       x = expression(paste(n[1], ",", n[2])))

ggsave("figures/speed_big_100000_inference_time2.png")
ggsave("../vabl/stocastic-vabl/jrssa/figures/speed_big_100000_inference_time.png")

results %>%
  mutate(sec_per_iter = time / iterations) %>%
  ggplot() +
  aes(x = n1, color = method, y = sec_per_iter) +
  geom_line(size = .8) +
  theme_bw() +
  labs(y = "Second",
       x = "Size of Each Dataset")

ggsave("figures/speed_big_100000_speed_per_iter.png")


results %>%
  filter(method %in% c("vabl", "svi")) %>%
  ggplot() +
  aes(x = n1, color = method, y = iterations) +
  geom_line(size = .8) +
  theme_bw() +
  labs(title = "Required vabl iterations")
#
#
# ggsave("figures/vabl_iterations_init.png")
