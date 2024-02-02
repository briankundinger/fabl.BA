library(tidyverse)

files <- list.files("out/sadinle_sim/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .) %>%
  filter(method != "BRL",
         method != "BRL_partial")

# results$precision[results$recall == 0] <- 0
# results$`f-measure`[results$recall == 0] <- 0
#
for_plot <- unique(results$method)[c(2, 4, 6, 8)]

results_df <- results %>%
  select(-RR, -time) %>%
  filter(method %in% for_plot) %>%
  mutate(method = case_when(
    method == "brl_hash" ~ "BRL",
    method == "svi" ~ "svabl",
    TRUE ~ method
  )) %>%
  mutate(method = factor(method, c("BRL", "fabl", "vabl", "svabl"))) %>%
  pivot_longer(cols = 1:3, names_to = "metric") %>%
  group_by(method, metric, error, overlap) %>%
  summarize(avg = mean(value, na.rm = T),
            median = quantile(value, .5, na.rm = T),
            lower = quantile(value, .025, na.rm = T),
            upper = quantile(value, .975, na.rm = T),
            .groups = "drop") %>%
  mutate(metric = factor(metric, c("recall", "precision", "f-measure")),
         error = factor(error, c("One Error", "Two Errors", "Three Errors")),
         overlap_percent = case_when(
           overlap == 50 ~ "10% Overlap",
           overlap == 250 ~ "50% Overlap",
           overlap == 450 ~ "90% Overlap"
         ),
         overlap_percent = factor(overlap_percent,
                                  c("90% Overlap", "50% Overlap", "10% Overlap")))



results_df %>%
  filter(metric != "f-measure") %>%
  ggplot(aes(x = metric, y = median,
             ymin  = lower, ymax = upper,
             color = method)) +
  geom_pointrange(position = position_dodge2(width = .7),
                  size = .1, fatten = 1) +
  facet_grid(overlap_percent ~ error) +
  labs(x = NULL,
       y = NULL,
       color = "Method") +
  scale_y_continuous() +
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_color_brewer(palette="Set1")

ggsave("figures/sadinle_sim.png")
ggsave("../vabl/stocastic-vabl/jrssa/figures/sadinle_sim.png")

for_plot <- unique(results$method)[c(3, 5, 7, 9)]

results_df <- results %>%
  rename(NPV = recall) %>%
  rename(PPV = precision) %>%
  mutate(DR = 1 - RR) %>%
  select(-time, -`f-measure`, -RR) %>%
  filter(method %in% for_plot) %>%
  mutate(method = case_when(
    method == "brl_hash_partial" ~ "BRL",
    method == "svi_partial" ~ "svabl",
    method == "vabl_partial" ~ "vabl",
    method == "fabl_partial" ~ "fabl",
    TRUE ~ method
  )) %>%
  mutate(method = factor(method, c("BRLhash", "fabl", "vabl", "svabl"))) %>%
  pivot_longer(cols = c(1, 2, 6), names_to = "metric") %>%
  group_by(method, metric, error, overlap) %>%
  summarize(avg = mean(value, na.rm = T),
            median = quantile(value, .5, na.rm = T),
            lower = quantile(value, .025, na.rm = T),
            upper = quantile(value, .975, na.rm = T),
            .groups = "drop") %>%
  mutate(metric = factor(metric, c("NPV", "PPV", "DR")),
         error = factor(error, c("One Error", "Two Errors", "Three Errors")),
         overlap_percent = case_when(
           overlap == 50 ~ "10% Overlap",
           overlap == 250 ~ "50% Overlap",
           overlap == 450 ~ "90% Overlap"
         ),
         overlap_percent = factor(overlap_percent,
                                  c("90% Overlap", "50% Overlap", "10% Overlap")))



results_df %>%
  ggplot(aes(x = metric, y = round(median, 3),
             ymin  = round(lower, 3), ymax = round(upper, 3),
             color = method)) +
  geom_pointrange(position = position_dodge2(width = .7),
                  size = .1, fatten = 1) +
  facet_grid(overlap_percent ~ error) +
  labs(x = NULL,
       y = NULL,
       color = "Method") +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_color_brewer(palette="Set1")

ggsave("figures/sadinle_sim_partial.png")
ggsave("../vabl/stocastic-vabl/jrssa/figures/sadinle_sim_partial.png")

