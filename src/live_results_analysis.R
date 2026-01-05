library(tidyverse)
live_results <- function(out_dir = "output/live_results") {
  fs <- list.files(out_dir, pattern = "^results_.*\\.rds$", full.names = TRUE)
  if (length(fs) == 0) return(data.frame())
  do.call(rbind, lapply(fs, readRDS))
}

res_so_far <- live_results()
print(res_so_far)
#View(res_so_far)
means <- res_so_far %>%
  group_by(n, data, sigma, a, sparsity) %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE),
    .groups = "drop"
  ) 
means


plot_df <- res_so_far %>%
  pivot_longer(
    cols = matches("(^probcor_|(_mse|_bias_mn|_bias)$)"),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    grouping = case_when(
      str_ends(metric, "_prob_mse") ~"prob_mse",
      str_ends(metric, "_mse") ~ "mse",
      str_ends(metric, "_bias_mn") ~ "bias_mn",
      str_ends(metric, "_bias") ~ "bias",
      str_starts(metric, "probcor_") ~ "probcor",
      TRUE ~ NA_character_
    ),
    # model is everything before the suffix
    model = metric %>%
      str_replace("_bias_mn$", "") %>%
      str_replace("_mse$", "") %>%
      str_replace("_bias$", "") %>% 
      str_replace("probcor_$", ""),
    grouping = factor(grouping, levels = c("mse", "bias", "bias_mn", "probcor"))
  ) %>%
  filter(!is.na(grouping), is.finite(value), model !="rf_uncali")

ggplot(plot_df %>% 
         filter(grouping == "bias_mn" &
                  (grepl("bart", model)|grepl("probit", model))), 
       aes(x = factor(n), y = abs(value), colour = model)) +
  geom_boxplot(outlier.alpha = 0.25, position = "dodge") +
  #geom_point() +
  facet_grid( sigma~data, scales = "free") +
  labs(x = "Model", y = "Bias") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  scale_colour_brewer(palette = "Paired")

ggplot(plot_df %>% 
         filter(grouping == "mse" &
                  (grepl("bart", model)|grepl("probit", model))), 
       aes(x = factor(n), y = abs(value), colour = model)) +
  geom_boxplot(outlier.alpha = 0.25, position = "dodge") +
  #geom_point() +
  facet_grid( sigma~data, scales = "free") +
  labs(x = "Model", y = "RMSE") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  scale_colour_brewer(palette = "Paired")



ggplot(plot_df %>% 
         filter(grouping == "bias_mn" &
                  (grepl("bart", model)|grepl("probit", model))) %>% 
         group_by(model, n, sigma, data) %>% 
         summarise(value = mean(abs(value))), 
       aes(x = n, y = abs(value), colour = model)) +
  geom_line() +
  geom_point() +
  facet_grid(sigma~data, scales = "free") +
  labs(x = "n", y = "Bias") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  scale_colour_brewer(palette = "Paired")

ggplot(plot_df %>% 
         filter(grouping == "mse" &
                  (grepl("bart", model)|grepl("probit", model))) %>% 
         group_by(model, n, sigma, data) %>% 
         summarise(value = mean(value)), 
       aes(x = n, y = abs(value), colour = model)) +
  geom_line() +
  geom_point() +
  facet_grid(sigma~data, scales = "free") +
  labs(x = "n", y = "RMSE") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  scale_colour_brewer(palette = "Paired")




library(tidyverse)

ttest_mse <- res_so_far %>%
  select(n, data, sigma, sparsity, a, rep, matches("((_mse|_bias_mn)$)")) %>% 
  group_by(n, data, sigma, sparsity, a) %>%
  summarise(
    n_pairs = n(),
    mean_diff = mean(probit_mse - bart_uncali_mse),
    sd_diff = sd(probit_mse - bart_uncali_mse),
    t_test = list(t.test(probit_mse, bart_uncali_mse, paired = TRUE)), #positive test stat means bart better
    .groups = "drop"
  ) %>%
  mutate(
    t_stat = map_dbl(t_test, ~ .x$statistic),
    direction = ifelse(t_stat > 0, "bart", "probit"), #favors bart or probit
    p_value = map_dbl(t_test, ~ .x$p.value)
  ) %>% mutate(    sig = p_value < 0.025)



ttest_bias <- res_so_far %>%
  select(n, data, sigma, sparsity, a, rep, matches("((_mse|_bias_mn)$)")) %>% 
  group_by(n, data, sigma, sparsity, a) %>%
  summarise(
    n_pairs = n(),
    probit_mean = mean(probit_bias_mn),
    bart_mean = mean(bart_uncali_bias_mn),
    mean_diff = mean(abs(probit_bias_mn) - abs(bart_uncali_bias_mn)),
    sd_diff = sd(abs(probit_bias_mn) - abs(bart_uncali_bias_mn)),
    t_test = list(t.test(abs(probit_bias_mn), abs(bart_uncali_bias_mn), paired = TRUE)), #positive test stat means bart better
    .groups = "drop"
  ) %>%
  mutate(
    t_stat = map_dbl(t_test, ~ .x$statistic),
    direction = ifelse(t_stat > 0, "bart", "probit"), #favors bart or probit
    p_value = map_dbl(t_test, ~ .x$p.value)
  ) %>% mutate(    sig = p_value < 0.025)



bias_tab <- ttest_bias %>% 
  group_by(sig, direction) %>% 
  count() %>%
  mutate(
    sig = ifelse(sig, "Significant", "Not significant"),
    direction = ifelse(direction == "bart", "BART", "Probit")
  ) %>%
  pivot_wider(
    names_from = direction,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(sig) %>%
  tibble::column_to_rownames("sig") %>%
  as.matrix()

bias_mat_margins <- addmargins(bias_tab)

mse_tab <- ttest_mse %>% 
  group_by(sig, direction) %>% 
  count()%>%
  mutate(
    sig = ifelse(sig, "Significant", "Not significant"),
    direction = ifelse(direction == "bart", "BART", "Probit")
  ) %>%
  pivot_wider(
    names_from = direction,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(sig) %>%
  tibble::column_to_rownames("sig") %>%
  as.matrix()

mse_mat_margins <- addmargins(mse_tab)


library(xtable)

xt_bias <- xtable(
  bias_mat_margins,
  caption = "Bias (mean-based)",
  label = "tab:ttest_bias"
)

xt_mse <- xtable(
  mse_mat_margins,
  caption = "RMSE",
  label = "tab:ttest_mse"
)


cat(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Counts of simulation scenarios by t-test significance and direction of advantage (BART vs Probit).}\n",
  "\\label{tab:ttest_counts}\n",
  "\\begin{minipage}[t]{0.48\\textwidth}\n",
  "\\centering\n"
)

print(xt_bias,
      include.rownames = TRUE,
      floating = FALSE,
      booktabs = TRUE)

cat(
  "\\end{minipage}\\hfill\n",
  "\\begin{minipage}[t]{0.48\\textwidth}\n",
  "\\centering\n"
)

print(xt_mse,
      include.rownames = TRUE,
      floating = FALSE,
      booktabs = TRUE)

cat(
  "\\end{minipage}\n",
  "\\end{table}\n"
)
