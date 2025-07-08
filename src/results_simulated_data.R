

all_combinedTRUE <- readRDS( paste0("output/all_combined_",TRUE,".RDS")) %>% mutate(kind = "Sparse")
all_combinedFALSE <- readRDS( paste0("output/all_combined_",FALSE,".RDS")) %>% mutate(kind = "Not Sparse")
all_combined <- rbind(all_combinedTRUE, all_combinedFALSE)


results_combinedTRUE <- readRDS( paste0("output/results_combined_",TRUE,".RDS")) %>% mutate(kind = "Sparse")
results_combinedFALSE <- readRDS( paste0("output/results_combined_",FALSE,".RDS")) %>% mutate(kind = "Not Sparse")
results_combined <- rbind(results_combinedTRUE, results_combinedFALSE)


results_combined <- results_combined %>% 
  select(n, data, sigma,a,kind, rep, bart_uncali_mse, nn2_mse, rf_uncali_mse, probit_mse, bprobit_mse,
         bart_uncali_bias, nn2_bias, rf_uncali_bias, probit_bias, bprobit_bias,
         bart_uncali_bias_mn, nn2_bias_mn, rf_uncali_bias_mn, probit_bias_mn, bprobit_bias_mn,
         mean_true, mean_bart, mean_probit
         ) %>% 
  mutate(data = factor(data, levels = c("normal", "friedman", "step", "bin"),
                       labels = c("Linear", "Friedman", "Step", "Binary"))
  ) 


# COMPARING RMSE - need to also look at bias
p <- rmse_ratio_plot(results_combined, k = "Sparse", sig = 7) + labs(caption="Sigma = 7, sparse coefficient vector");p
ggsave(paste0("RMSE_ratios_",TRUE,"_7",".pdf"),plot = p, width = 15, height = 10)

p <- rmse_ratio_plot(results_combined, k = "Sparse", sig = 15) + labs(caption="Sigma = 15, sparse coefficient vector");p
ggsave(paste0("RMSE_ratios_",TRUE,"_15",".pdf"),plot = p, width = 15, height = 10)

p <- rmse_ratio_plot(results_combined, k = "Not Sparse", sig = 7)+ labs(caption="Sigma = 7, full coefficient vector");p
ggsave(paste0("RMSE_ratios_",FALSE,"_7",".pdf"),plot = p, width = 20, height = 15)

p <- rmse_ratio_plot(results_combined, k = "Not Sparse", sig = 15)+ labs(caption="Sigma = 15, full coefficient vector");p
ggsave(paste0("RMSE_ratios_",FALSE,"_15",".pdf"),plot = p, width = 20, height = 15)

#LRF TO DO: MAKE PROBIT A DASHED LINE
p <- rmse_plot(results_combined, k = "Sparse", sig = 7)# + labs(caption="Sigma = 7, sparse coefficient vector");p
ggsave(paste0("RMSE_",TRUE,"_7",".pdf"),plot = p, width = 20, height = 15)

p <- rmse_plot(results_combined, k = "Sparse", sig = 15) #+ labs(caption="Sigma = 15, sparse coefficient vector");p
ggsave(paste0("RMSE_",TRUE,"_15",".pdf"),plot = p, width = 20, height = 15)

p <- rmse_plot(results_combined, k = "Not Sparse", sig = 7)#+ labs(caption="Sigma = 7, full coefficient vector");p
ggsave(paste0("RMSE_",FALSE,"_7",".pdf"),plot = p, width = 20, height = 15)

p <- rmse_plot(results_combined, k = "Not Sparse", sig = 15)#+ labs(caption="Sigma = 15, full coefficient vector");p
ggsave(paste0("RMSE_",FALSE,"_15",".pdf"),plot = p, width = 20, height = 15)


##PRECISION - machine learning algorithms can be less stable
#mean absolute error
p <- mae_plot(results_combined, k="Sparse", sig = 7)+ ggtitle("Sparse");p
ggsave(paste0("mae_mean_", TRUE,".pdf"), width = 20, height = 15)
p <- mae_plot(results_combined, k="Not Sparse", sig = 15)+ ggtitle("Not Sparse");p
ggsave(paste0("mae_mean_", FALSE, ".pdf"), width = 20, height = 15)

#mean absolute error ratio
p <- mae_ratio_plot(results_combined, k = "Sparse", sig = 7) + ggtitle("Sparse");p
ggsave(paste0("MAE_ratios_",TRUE,".pdf"),plot = p, width = 20, height = 15)
p <- mae_ratio_plot(results_combined, k = "Sparse", sig = 15) + ggtitle("Sparse");p
ggsave(paste0("MAE_ratios_",TRUE,".pdf"),plot = p, width = 20, height = 15)
p <- mae_ratio_plot(results_combined, k = "Not Sparse", sig = 15)+ ggtitle("Not Sparse");p
ggsave(paste0("MAE_ratios_",FALSE,".pdf"),plot = p, width = 20, height = 15)


## BIAS
p <- bias_plot(results_combined, k="Sparse", sig = 15)+ ggtitle("Sparse");p
ggsave(paste0("bias_mean_", TRUE,".pdf"), width = 20, height = 15)
p <- bias_plot(results_combined, k="Not Sparse", sig = 15)+ ggtitle("Not Sparse");p
ggsave(paste0("bias_mean_", FALSE, ".pdf"), width = 20, height = 15)


dcurves <- all_combined%>% 
  select(n, data,kind, sigma,a, true, bart_uncali_q, nn2_q, rf_uncali, probit, bprobit) %>% 
  group_by(a, n, data,kind, sigma) %>%
  mutate_at(c("true","bart_uncali_q", "nn2_q", "rf_uncali", "probit", "bprobit"), list(quant = ~ 1-ecdf(.)(.))) %>%
  ungroup() 

dcurves %>% 
  group_by(a,kind, n, data, sigma) %>% 
  summarise(probit = ks.test(probit_quant, true_quant, alternative = "two.sided")$ p.value,
            bart = ks.test(bart_uncali_q_quant, true_quant, alternative = "two.sided")$ p.value)


dcurves %>% 
  mutate(data = factor(data, levels = c("normal", "friedman", "step", "bin"),
                       labels = c("Linear", "Friedman", "Step", "Binary"))
  ) %>% 
  filter(sigma == 15 & 
           a == "symmetric" &
           kind == "Not Sparse" & 
           n ==1500 ) %>% 
  ggplot() + 
  geom_line(aes(x = bart_uncali_q,y = bart_uncali_q_quant, colour = "BART" )) +
  geom_line(aes(x = probit,y = probit_quant, colour = "Probit" )) +
  geom_line(aes(x = nn2_q,y = nn2_q_quant, colour = "NN" )) +
  geom_line(aes(x = true,y = true_quant, colour = "True"),linewidth = 1) +
  facet_grid(.~data, scales = "free") +
  scale_color_manual(
    name = "Model", 
    breaks = c("True","BART", "NN", "RF", "Probit"),
    values = c("True" = "red",
               "BART" = "black",
               "NN" = "grey40",
               "RF" = "grey60",
               "Probit" = "grey84")
  ) +
  scale_linewidth_manual("Model", values = c(1, rep(.5, 4)))+ 
  theme_bw() +
  labs(x = "WTP", y = "Quantile")

ggsave(paste0("curves_", sparsity, ".pdf"), width = 20, height = 20)




all_combined %>% 
  filter(sigma == 7 & a == "symmetric" & kind == "Not Sparse" & n ==1000 ) %>% 
  ggplot() + 
  geom_point(aes(x = true, y = bart_uncali_q)) +
  geom_abline(aes(slope = 1, intercept = 0)) +
  facet_wrap(~data, scales = "free")

all_combined %>% 
  filter(sigma == 7 & a == "symmetric" & kind == "Not Sparse" & n ==1000 ) %>% 
  ggplot() + 
  geom_point(aes(x = true, y = bprobit)) +
  geom_abline(aes(slope = 1, intercept = 0))+
  facet_wrap(~data, scales = "free")

  

ggplot(data = results_combined %>% 
         filter(a == "symmetric" & kind == "Sparse")) + 
  geom_point(aes(x = mean_probit, y = mean_bart, colour = n)) + 
  facet_wrap(data~., scales = "free") + 
  geom_abline(aes(slope = 1, intercept = 0))
