

get_wtp <- function(pred_matrix){

  test_ends[paste0("Rep", 1:4000)] <- NA
  
  test_ends[paste0("Rep", 1:4000)] <- lapply(test_ends[paste0("Rep", 1:4000)], function(x) {
    ifelse(test_ends$CV_donate_bid == max(pts), 0, 1)
  })

  
  pred_matrix2 <- data.frame(pred_matrix) 
  
  colnames(pred_matrix2) <- gsub("X", "Rep", colnames(pred_matrix2))
  print("two")
  testb <- test_notends %>% 
    data.frame(pred_matrix2)#(1-mb$prob.test) %>%  t() 


  # Save original data
  original_tdat <- testb %>%
    rbind(test_ends) %>%
    group_by(ID) %>%
    arrange(CV_donate_bid, .by_group = TRUE)
  
  # monotonic adjustment
  tdat <- original_tdat %>%
    mutate(across(starts_with("Rep"), ~ pava(.x, decreasing = TRUE)))
  
  #diff <- data.frame(original_tdat[,c(1:6)],
   #                   abs(original_tdat[,-c(1:6)] - tdat[,-c(1:6)])) %>% 
  #group_by(ID) %>% 
    #summarise(anydiff = any(Rep1 != 0),
    #          countdiff = sum(Rep1 != 0)/2)
  
#table(diff$countdiff)/nrow(diff)

  
 #### END COUNTING
  
  
  tdat <- tdat %>% select("ID", "CV_donate_bid",paste0("Rep", 1:4000))
  

  tdat_long <- tdat %>%
    pivot_longer(cols = starts_with("Rep"), names_to = "variable", values_to = "value")
  print("four")
  #calculate weighted sum
  #for negative differences (non-monotonicity), give a value of 0
  tdat_long2 <- tdat_long%>% 
    group_by(ID,variable) %>% #hh_income, variable, age
    summarise(wtp_q = integrate(approxfun(CV_donate_bid[!is.na(value)],value[!is.na(value)]), 0, max(test_ends$CV_donate_bid))$value)# %>% 
  print("five")
  mean_wtp <-tdat_long2 %>% 
    ungroup %>% 
    group_by(ID) %>% 
    summarise(wtp_q = mean(wtp_q))
  
return(list(wtp=tdat_long2, mean_wtp = mean_wtp))
  
}


get_wtp2 <- function(pred_matrix, test_ends,test_notends, ndpost){
  pts <- unique(test_ends$A)
  max_pts <- max(pts)
  print("first")
  # Create a matrix instead of repeatedly assigning NA columns
  test_ends[paste0("Rep", 1:ndpost)] <- as.numeric(test_ends$A != max_pts)
  
  pred_matrix2 <- data.frame(pred_matrix) 
  colnames(pred_matrix2) <- gsub("X", "Rep", colnames(pred_matrix2))
  print("sec")
  testb <- test_notends %>% 
    data.frame(pred_matrix2)#(1-mb$prob.test) %>%  t() 
  #colnames(testb) <- gsub("X", "Rep", colnames(testb))
  
  print("third")
  tdat <-testb %>%
    rbind(test_ends) %>% #tack on endpoints
    group_by(ID) %>% 
    arrange(A, .by_group = TRUE) %>% 
    mutate(across(starts_with("Rep"), ~  pava(.x, decreasing=TRUE))) %>% 
    ungroup() #%>% 
    #arrange(ID, A)
  print("fourth")
 # View(tdat[,c(1:3,13,1013)])

  tdat <- tdat %>% select("ID", "A",paste0("Rep", 1:ndpost))
  
  print("fifth")
  
  tdat_long <- tdat %>%
    pivot_longer(cols = starts_with("Rep"), names_to = "variable", values_to = "value")
  print("six")
  #calculate weighted sum
  #for negative differences (non-monotonicity), give a value of 0
  # Precompute unique 'A' values and their max to avoid recalculating in every group
  max_A <- max(tdat_long$A, na.rm = TRUE) 
  
  # Create an optimized pipeline
  tdat_long2 <- tdat_long %>%
    filter(!is.na(value)) %>% # Avoid recalculating NA checks multiple times
    group_by(ID, variable) %>%
    summarise(
      wtp_q = {
        fun <- approxfun(A, value) # Create the interpolation function once
        integrate(fun, 0, max_A)$value
      },
      .groups = "drop" # Explicitly drop grouping for better performance
    ) %>%
    group_by(ID) %>%
    summarise(wtp_q = mean(wtp_q), .groups = "drop")
  print("seven")
  return(tdat_long2)
  
}

# 
# temp =tdat_long%>% 
#   group_by(ID,variable) %>% #hh_income, variable, age
#   mutate(CDF = c(1,1-cumsum(value))[-length(value)])
# 
# 
# temp2 <- temp %>% 
#   filter(ID %in% c(3) & variable %in% paste0("diff_Rep",1:5))   
# 
# temp3 <- tdat_long2 %>% filter(ID %in% c(3) & variable %in% paste0("diff_Rep",1:5))
# 
#   ggplot() + 
#     geom_line(aes(x = rep(1:10, 5), group = variable, y = CDF, colour = variable), data = temp2 %>% filter(CV_donate_bid > 0)) +
#     geom_text(aes(x = 10, y = CDF,colour = variable, label = paste0("Area = ", round(temp3$wtp_q,1))),data = temp2 %>% filter(CV_donate_bid ==2000), hjust = .3, size = 6)+
#     theme_bw() +scale_colour_grey() +
#     labs(x="A", y = "P(Y = 1 | A)") +
#     theme(legend.position = "none", axis.text.x = element_blank(),
#           axis.text.y = element_blank(),
#           text = element_text(size = 25)) +
#     expand_limits(x=c(0,11.5))
# ggsave("integral_mcmc.pdf")



get_wtp_point <- function(pred_vector, test_ends,test_notends){
  pts <- unique(test_ends$A)
  test_ends$Rep1 <- NA
  
  test_ends$Rep1 <-ifelse(!test_ends$A == max(pts), 1, 0)

  
  testb <- test_notends %>% 
    data.frame(Rep1 = pred_vector)#(1-mb$prob.test) %>%  t() 
  #colnames(testb) <- gsub("X", "Rep", colnames(testb))
  
  
  
  tdat <-testb %>%
    rbind(test_ends) %>% #tack on endpoints
    group_by(ID) %>% 
    arrange(ID, (A)) %>% 
    # calculate differences in consecutive probabilities
    # (some will be negative, that is taken care of below)
    mutate(across(starts_with("Rep"), ~  pava(.x, decreasing=TRUE))) %>% 
    arrange(ID, A)
  
  
  tdat <- tdat %>% select("ID", "A",paste0("Rep1"))
  
  
  
  tdat_long <- tdat %>% 
    #select(-starts_with("Rep")) %>% 
    melt(id.vars = c("ID", "A"))# %>% 
  #filter(!is.na(hh_income))
  max_A<- max(tdat_long$A, na.rm = TRUE) 
  
  tdat_long2 <- tdat_long%>% 
    group_by(ID,variable) %>% #hh_income, variable, age
    summarise(wtp_q = integrate(approxfun(A[!is.na(value)],value[!is.na(value)]), 0, max_A)$value) %>% 
    ungroup %>% 
    group_by(ID) %>% 
    summarise(wtp_q = mean(wtp_q))
  
  #TOMORROW: need to have CDF = 1 for A = 0. (i.e., shift the CDF values down by one row)
  #TOMORROW: check SK computations - why is it biased downward
  # ---> don't use SK. use quadrature instead (which is what SK wants to estimate anyway). 
  #TOMORROW: instead of using pmax, try combining bins when there is non-monotonicity
  
  return(tdat_long2)
  
}

make_bins <- function(df){
  df <- df %>%
    mutate(across(starts_with("X."), ~ ifelse(. > 0.5, 1, 0)))
  return(df)
}



rmse_plot <- function(results_combined, sym, sig){ 
results_combined %>%
    filter(sigma == sig & a == sym) %>%  #for a given sigma and x symmetry
    melt(id.vars = c("data", "rep", "n", "a", "kind", "sigma", "mean_true")) %>%
    filter(grepl("mse", variable)) %>%
  mutate(variable = factor(variable, 
                           levels = c("bart_uncali_mse","bart_tuned_mse", "nn2_mse", "rf_uncali_mse", "bprobit_mse", "probit_mse"),
                           labels = c("BART", "BART Tuned", "NN", "RF", "Bayesian Probit", "Probit"))) %>% 
  group_by(data, kind, n, variable) %>% 
  summarise(value = mean(value)/mean(mean_true)) %>% 
  ggplot() +
  geom_line(aes(x = n, y = value, colour = factor(variable), group = factor(variable), linetype = factor(variable)), linewidth = 1.25) +
  geom_point(aes(x = n, y = value, colour = factor(variable)), size = 2) +
    scale_linetype_manual("Model", values = c("solid", "solid","dotted", "dashed","solid", "dashed")) +
    scale_colour_manual("Model", values = paste0("grey", c(10, 30, 40, 50, 70, 90))) +
  facet_grid(kind~data, scales = "free_y") +
  labs(x = "n", y = "RMSE (% of true WTP mean)") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  #  scale_colour_brewer("Model", type = "qual", palette = "Dark2") +
  theme_bw()+
    theme(text=element_text(size = 25),legend.key.width = unit(2, "cm") ) +
    scale_x_continuous(breaks = unique(results_combined$n))
}
  
  
cor_plot <- function(results_combined, sym, sig){
  results_combined %>%
    #filter(data != "Step") %>% 
    filter(sigma == sig & a == sym) %>% 
    melt(id.vars = c("data", "rep", "n", "a", "kind", "sigma")) %>% 
    filter(grepl("probcor", variable) ) %>% 
    mutate(variable = factor(variable, 
                             levels = c("probcor_bart", "probcor_bart_tuned", "probcor_nn", "probcor_rf", "probcor_bprobit","probcor_probit"),
                             labels = c("BART","BART Tuned", "NN", "RF", "Bayesian Probit", "Probit"))) %>% 
    group_by(data, kind, n, variable) %>% 
    summarise(value = mean(value)) %>% 
    ggplot() +
    geom_line(aes(x = n, y = value, colour = factor(variable), group = factor(variable), linetype = factor(variable)), linewidth = 1.25) +
    geom_point(aes(x = n, y = value, colour = factor(variable)), size = 2) +
    scale_linetype_manual("Model", values = c("solid", "solid","dotted", "dashed","solid", "dashed")) +
    scale_colour_manual("Model", values = paste0("grey", c(10, 30, 40, 50, 70, 90))) +
    facet_grid(kind~data, scales = "free_y") +
    labs(x = "Sample size (n)", y = "Correlation") +
    #scale_colour_grey("Model",start = 0, end = .8) +
    #  scale_colour_brewer("Model", type = "qual", palette = "Dark2") +
    theme_bw()+
    theme(text=element_text(size = 25),legend.key.width = unit(2, "cm") ) +
    scale_x_continuous(breaks = unique(results_combined$n))
}


bias_plot <- function(results_combined, sym, sig){
results_combined %>%
  filter(sigma == sig & a == sym) %>% 
  select("n" ,"data" ,"sigma", "a" ,"kind","rep","mean_true","mean_bart" ,"mean_probit", "mean_bprobit" ,"mean_bart_tuned" , "mean_nn","mean_rf_uncali" ) %>% 
  pivot_longer(
    cols = starts_with("mean_") & !matches("mean_true"),
    names_to = "model",
    values_to = "mean_hat"
  ) %>%
  mutate(
    model = sub("^mean_", "", model),
    abs_bias = (mean_hat - mean_true)/mean_true
  ) %>%
  mutate(variable = factor(model, 
                           levels = c("bart", "bart_tuned", "nn", "rf_uncali", "bprobit","probit"),
                           labels = c("BART","BART Tuned", "NN", "RF", "Bayesian Probit", "Probit"))) %>% 
  group_by(n, data, kind, variable) %>%
  summarise(
    mean_abs_bias = mean(abs_bias),
    .groups = "drop"
  ) %>% 
  ggplot( aes(x = n, y = mean_abs_bias, color = variable, linetype = variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
    scale_linetype_manual("Model", values = c("solid", "solid","dotted", "dashed","solid", "dashed")) +
    scale_colour_manual("Model", values = paste0("grey", c(10, 30, 40, 50, 70, 90))) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  facet_grid(kind ~ data) +
  labs(
    x = "Sample size (n)",
    y = "Bias (% of true WTP mean)",
    color = "Model"
  ) +
  theme_bw(base_size = 13) +
  theme(text=element_text(size = 25),legend.key.width = unit(2, "cm") ) 
}
  



get_tabs <- function(res_so_far){ #give it results_combined
  ttest_mse <- res_so_far %>%
    select(n, data, sigma, kind, a, rep, matches("((_mse|_bias_mn)$)")) %>% 
    group_by(n, data, sigma, kind, a) %>%
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
    select(n, data, sigma, kind, a, rep, matches("((_mse|_bias_mn)$)")) %>% 
    group_by(n, data, sigma, kind, a) %>%
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
 
  return(list( 
  xt_bias = xtable(
    bias_mat_margins,
    caption = "Bias (mean-based)",
    label = "tab:ttest_bias"
  )
  ,
  xt_mse = xtable(
    mse_mat_margins,
    caption = "RMSE",
    label = "tab:ttest_mse"
  )))
  
}
