library(tidyverse)

files <- list.files("out/speed_big2/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .)

results %>%
  ggplot() +
  aes(x = n1, color = method, y = time) +
  geom_line(linewidth = .8) +
  theme_bw() +
  labs(y = "Inference Time (Seconds)",
       x = expression(n[1]))

ggsave("figures/speed_big2_100000_inference_time_all.png")
ggsave("../vabl/stocastic-vabl/jrssa/figures/speed_big2_100000_inference_time_all.png")


results %>%
  ggplot() +
  aes(x = n1, color = method, y = time) +
  geom_line(linewidth = .8) +
  theme_bw() +
  labs(y = "Inference Time (Seconds)",
       x = expression(n[1])) +
  ylim(0, 86)

ggsave("figures/speed_big2_100000_inference_time.png")
ggsave("../vabl/stocastic-vabl/jrssa/figures/speed_big2_100000_inference_time.png")

results %>%
  filter(method != "BRL") %>%
  ggplot() +
  aes(x = n1, color = method, y = time) +
  geom_line(linewidth = .8) +
  theme_bw() +
  labs(y = "Inference Time (Seconds)",
       x = expression(n[1]))

ggsave("figures/speed_big2_100000_inference_time_no_BRL.png")
ggsave("../vabl/stocastic-vabl/jrssa/figures/speed_big2_no_BRL.png")



results %>%
  mutate(sec_per_iter = time / iterations) %>%
  ggplot() +
  aes(x = n1, color = method, y = sec_per_iter) +
  geom_line(size = .8) +
  theme_bw() +
  labs(y = "Second",
       x = "Size of Larger Dataset")

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
