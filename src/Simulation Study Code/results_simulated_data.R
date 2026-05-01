

all_combinedTRUE <- readRDS( paste0("output/all_combined_",TRUE,"2.RDS")) %>% mutate(kind = "Sparse")
all_combinedFALSE <- readRDS( paste0("output/all_combined_",FALSE,"2.RDS")) %>% mutate(kind = "Not Sparse")
all_combined <- rbind(all_combinedTRUE, all_combinedFALSE)


results_combinedTRUE <- readRDS( paste0("output/results_combined_",TRUE,"2.RDS")) %>% mutate(kind = "Sparse")
results_combinedFALSE <- readRDS( paste0("output/results_combined_",FALSE,"2.RDS")) %>% mutate(kind = "Not Sparse")
results_combined <- rbind(results_combinedTRUE, results_combinedFALSE)


results_combined <- results_combined %>% 
  select(n, data, sigma,a,kind, rep, bart_uncali_mse,bart_tuned_mse, nn2_mse, rf_uncali_mse, probit_mse, bprobit_mse,
         bart_uncali_bias,bart_tuned_bias, nn2_bias, rf_uncali_bias, probit_bias, bprobit_bias,
         bart_uncali_bias_mn,bart_tuned_bias_mn, nn2_bias_mn, rf_uncali_bias_mn, probit_bias_mn, bprobit_bias_mn,
         probcor_bart,probcor_bart_tuned, probcor_nn, probcor_rf, probcor_probit, probcor_bprobit,
         mean_true, mean_bart, mean_probit, mean_bprobit, mean_bart_tuned, mean_nn, mean_rf_uncali,
         ) %>% 
  mutate(data = factor(data, levels = c("normal", "friedman", "step", "bin"),
                       labels = c("Linear", "Friedman", "Step", "Binary"))
  ) 


#CORRELATION PLOT

p <- cor_plot(results_combined, sym = "symmetric", sig = 7);p# + labs(caption="Sigma = 7, sparse coefficient vector");p
ggsave(paste0("cor_","sym","_7",".pdf"),plot = p, width = 20, height = 15)
#small sigma, sparse --> bart benefits from tuning in step and binary

p <- cor_plot(results_combined, sym = "symmetric", sig = 15);p #+ labs(caption="Sigma = 15, sparse coefficient vector");p
ggsave(paste0("cor_","sym","_15",".pdf"),plot = p, width = 20, height = 15)
#benefit of tuning no longer there when sigma increases

p <- cor_plot(results_combined, sym = "asymmetric", sig = 7);p#+ labs(caption="Sigma = 7, full coefficient vector");p
ggsave(paste0("cor_","asym","_7",".pdf"),plot = p, width = 20, height = 15)
#again, for small sigma non-sparse situations, benefit to tuning bart, though mostly for binary

p <- cor_plot(results_combined, sym = "asymmetric", sig = 15);p#+ labs(caption="Sigma = 15, full coefficient vector");p
ggsave(paste0("cor_","asym","_15",".pdf"),plot = p, width = 20, height = 15)


#RMSE PLOT
#LRF TO DO - DIVIDE BY AVERAGE WTP
p <- rmse_plot(results_combined, sym = "symmetric", sig = 7);p# + labs(caption="Sigma = 7, sparse coefficient vector");p
ggsave(paste0("RMSE_","sym","_7",".pdf"),plot = p, width = 20, height = 15)

p <- rmse_plot(results_combined, sym = "symmetric", sig = 15);p #+ labs(caption="Sigma = 15, sparse coefficient vector");p
ggsave(paste0("RMSE_","sym","_15",".pdf"),plot = p, width = 20, height = 15)

p <- rmse_plot(results_combined, sym = "asymmetric", sig = 7);p#+ labs(caption="Sigma = 7, full coefficient vector");p
ggsave(paste0("RMSE_","asym","_7",".pdf"),plot = p, width = 20, height = 15)

p <- rmse_plot(results_combined, sym = "asymmetric", sig = 15);p#+ labs(caption="Sigma = 15, full coefficient vector");p
ggsave(paste0("RMSE_","asym","_15",".pdf"),plot = p, width = 20, height = 15)


#BIAS PLOT

p <- bias_plot(results_combined, sym = "symmetric", sig = 7);p# + labs(caption="Sigma = 7, sparse coefficient vector");p
ggsave(paste0("bias_","sym","_7",".pdf"),plot = p, width = 20, height = 15)

p <- bias_plot(results_combined, sym = "symmetric", sig = 15);p #+ labs(caption="Sigma = 15, sparse coefficient vector");p
ggsave(paste0("bias_","sym","_15",".pdf"),plot = p, width = 20, height = 15)

p <- bias_plot(results_combined, sym = "asymmetric", sig = 7);p#+ labs(caption="Sigma = 7, full coefficient vector");p
ggsave(paste0("bias_","asym","_7",".pdf"),plot = p, width = 20, height = 15)

p <- bias_plot(results_combined, sym = "asymmetric", sig = 15);p#+ labs(caption="Sigma = 15, full coefficient vector");p
ggsave(paste0("bias_","asym","_15",".pdf"),plot = p, width = 20, height = 15)



#significance testing

ts <- get_tabs(results_combined)

ts$xt_bias
ts$xt_mse


dcurves <- all_combined%>% 
  select(n, data,kind, sigma,a, true, bart_uncali_q, nn2_q, rf_uncali, probit, bprobit) %>% 
  group_by(a, n, data,kind, sigma) %>%
  mutate_at(c("true","bart_uncali_q", "nn2_q", "rf_uncali", "probit", "bprobit"), list(quant = ~ 1-ecdf(.)(.))) %>%
  ungroup() 

dcurves %>% 
  group_by(a,kind, n, data, sigma) %>% 
  summarise(probit = ks.test(probit_quant, true_quant, alternative = "two.sided")$ p.value,
            bart = ks.test(bart_uncali_q_quant, true_quant, alternative = "two.sided")$ p.value)






all_combined %>% 
  filter(sigma == 7 & a == "symmetric" & kind == "Not Sparse" & n ==1000 ) %>% 
  ggplot() + 
  geom_point(aes(x = true, y = bart_uncali_q)) +
  geom_abline(aes(slope = 1, intercept = 0)) +
  facet_wrap(~data, scales = "free") +
  ggtitle("BART")

all_combined %>% 
  filter(sigma == 7 & a == "symmetric" & kind == "Not Sparse" & n ==1000 ) %>% 
  ggplot() + 
  geom_point(aes(x = true, y = bprobit)) +
  geom_abline(aes(slope = 1, intercept = 0))+
  facet_wrap(~data, scales = "free")+
  ggtitle("B Probit")

  

ggplot(data = results_combined %>% 
         filter(a == "symmetric" & kind == "Sparse")) + 
  geom_point(aes(x = mean_true, y = mean_probit, colour = n)) + 
  facet_wrap(data~., scales = "free") + 
  geom_abline(aes(slope = 1, intercept = 0))
