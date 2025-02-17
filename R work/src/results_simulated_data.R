

all_combinedTRUE <- readRDS( paste0("all_combined_",TRUE,".RDS")) %>% mutate(kind = "Sparse")
all_combinedFALSE <- readRDS( paste0("all_combined_",FALSE,".RDS")) %>% mutate(kind = "Not Sparse")
all_combined <- rbind(all_combinedTRUE, all_combinedFALSE)


results_combinedTRUE <- readRDS( paste0("results_combined_",TRUE,".RDS")) %>% mutate(kind = "Sparse")
results_combinedFALSE <- readRDS( paste0("results_combined_",FALSE,".RDS")) %>% mutate(kind = "Not Sparse")
results_combined <- rbind(results_combinedTRUE, results_combinedFALSE)


results_combined <- results_combined %>% 
  select(n, data, sigma,a,sparsity, rep, bart_uncali_mse, nn2_mse, rf_uncali_mse, probit_mse, bprobit_mse,
         bart_uncali_bias, nn2_bias, rf_uncali_bias, probit_bias, bprobit_bias) %>% 
  mutate(data = factor(data, levels = c("normal", "friedman", "step", "bin"),
                       labels = c("Linear", "Friedman", "Step", "Binary"))
  ) 


# COMPARING RMSE - need to also look at bias
p <- rmse_ratio_plot(results_combined, k = TRUE) + ggtitle("Sparse");p
ggsave(paste0("RMSE_ratios_",TRUE,".pdf"),plot = p, width = 20, height = 15)
p <- rmse_ratio_plot(results_combined, k = FALSE)+ ggtitle("Not Sparse");p
ggsave(paste0("RMSE_ratios_",FALSE,".pdf"),plot = p, width = 20, height = 15)


#bias
p <- bias_plot(results_combined, k=TRUE)+ ggtitle("Sparse");p
ggsave(paste0("bias_median_", TRUE,"_.pdf"), width = 20, height = 15)
p <- bias_plot(results_combined, k=FALSE)+ ggtitle("Not Sparse");p
ggsave(paste0("bias_median_", FALSE, "_.pdf"), width = 20, height = 15)

#mean absolute error
p <- mae_plot(results_combined, k=TRUE)+ ggtitle("Sparse");p
ggsave(paste0("mae_median_", TRUE,"_.pdf"), width = 20, height = 15)
p <- mae_plot(results_combined, k=FALSE)+ ggtitle("Not Sparse");p
ggsave(paste0("mae_median_", FALSE, "_.pdf"), width = 20, height = 15)


#LRF TO DO: MAKE PROBIT A DASHED LINE
#is this one necessary? it's just the first plot, but not relative to probit
p <- rmse_plot(results_combined,k=TRUE);p
ggsave(paste0("RMSE_", TRUE, ".pdf"), width = 20, height = 15)
p <- rmse_plot(results_combined,k=FALSE);p
ggsave(paste0("RMSE_", FALSE, ".pdf"), width = 20, height = 15)



dcurves <- all_combined%>% 
  select(n, data, sigma,a, true, bart_uncali_q, nn2_q, rf_uncali, probit, bprobit) %>% 
  group_by(a, n, data, sigma) %>%
  mutate_at(c("true","bart_uncali_q", "nn2_q", "rf_uncali", "probit", "bprobit"), list(quant = ~ 1-ecdf(.)(.))) %>%
  ungroup() 

dcurves %>% 
  group_by(a, n, data, sigma) %>% 
  summarise(probit = ks.test(probit_quant, true_quant, alternative = "two.sided")$ p.value,
            bart = ks.test(bart_uncali_q_quant, true_quant, alternative = "two.sided")$ p.value)


dcurves %>% 
  mutate(data = factor(data, levels = c("normal", "friedman", "step", "bin"),
                       labels = c("Linear", "Friedman", "Step", "Binary"))
  ) %>% 
  filter(sigma == 7 & a == "asymmetric"  ) %>% 
  ggplot() + 
  geom_line(aes(x = bart_uncali_q,y = bart_uncali_q_quant, colour = "BART" )) +
  geom_line(aes(x = probit,y = probit_quant, colour = "Probit" )) +
  geom_line(aes(x = nn2_q,y = nn2_q_quant, colour = "NN" )) +
  geom_line(aes(x = true,y = true_quant, colour = "True"),linewidth = 1) +
  facet_grid(n~data, scales = "free") +
  scale_color_manual(
    name = "Model", 
    breaks = c("True","BART", "NN", "RF", "Probit"),
    values = c("True" = "black",
               "BART" = "black",
               "NN" = "grey40",
               "RF" = "grey60",
               "Probit" = "grey84")
  ) +
  scale_linewidth_manual("Model", values = c(1, rep(.5, 4)))+ 
  theme_bw() +
  labs(x = "WTP", y = "Quantile")

ggsave(paste0("curves_", sparsity, ".pdf"), width = 20, height = 20)



