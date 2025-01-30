

get_wtp <- function(pred_matrix){
  
  print("first")
  test_ends[paste0("Rep", 1:1000)] <- NA
  
  test_ends[paste0("Rep", 1:1000)] <- lapply(test_ends[paste0("Rep", 1:1000)], function(x) {
    ifelse(test_ends$CV_donate_bid == max(pts), 0, 1)
  })
  print("one")
  
  pred_matrix2 <- data.frame(pred_matrix) 
  
  colnames(pred_matrix2) <- gsub("X", "Rep", colnames(pred_matrix2))
  print("two")
  testb <- test_notends %>% 
    data.frame(pred_matrix2)#(1-mb$prob.test) %>%  t() 

  
  tdat <-testb %>%
    rbind(test_ends) %>% #tack on endpoints
    group_by(ID) %>% 
    arrange(ID, (CV_donate_bid)) %>% 
    # calculate differences in consecutive probabilities
    # (some will be negative, that is taken care of below)
     mutate(across(starts_with("Rep"), ~  pava(.x, decreasing=TRUE))) #%>% #Rep = P(said no)
    #mutate(across(starts_with("Rep"), ~  lag(.)- ., .names = "diff_{col}")) %>% 
    #arrange(ID, CV_donate_bid)
  print("three")
  #View(tdat[,c(1,5,1005)])
  
  tdat <- tdat %>% select("ID", "CV_donate_bid",paste0("Rep", 1:1000))
  

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
  test_ends[paste0("Rep", 1:ndpost)] <- as.numeric(!test_ends$A == max_pts)
  
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
  
  tdat_long2 <- tdat_long%>% 
    group_by(ID,variable) %>% #hh_income, variable, age
    summarise(wtp_q = integrate(approxfun(A[!is.na(value)],value[!is.na(value)]), 0, 150)$value) %>% 
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



rmse_ratio_plot <- function(results_combined, k){
results_combined %>%
    filter(sigma == 7 & kind == k) %>% 
  group_by(data,n, kind) %>%
  summarise(
    #bart_cali = mean(bart_mse) / mean(probit_mse),
    bart_uncali = mean(bart_uncali_mse) / mean(probit_mse),
    nn2_q = mean(nn2_mse) / mean(probit_mse),
    #rf_cali = mean(rf_mse) / mean(probit_mse),
    rf_uncali = mean(rf_uncali_mse) / mean(probit_mse),
    bprobit = mean(bprobit_mse) / mean(probit_mse)
  ) %>% 
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = n, y = nn2_q, colour = "NN")) +
  geom_point(aes(x = n, y = nn2_q, colour = "NN")) +
  
  #geom_line(aes(x = sigma, y = bprobit, colour = "B. Probit")) +
  #geom_point(aes(x = sigma, y = bprobit, colour = "B. Probit")) +
  
  geom_line(aes(x = n, y = rf_uncali, colour = "RF")) +
  geom_point(aes(x = n, y = rf_uncali, colour = "RF")) +
  
  geom_line(aes(x = n, y = bart_uncali, colour = "BART")) +  
  geom_point(aes(x = n, y = bart_uncali, colour = "BART")) +
  
  geom_hline(aes(yintercept = 1, colour = "Probit"), linetype = 2) +
  
  facet_grid(a~data) +
  scale_color_manual(
    name = "Model", # Legend title
    #values = c("BART" = "grey10", "NN" = "grey50", "RF" = "grey80", "Probit" = "grey"),
    breaks = c("BART", "NN", "RF", "Probit"),
    values = c("BART" = "black",
               "NN" = "grey40",
               "RF" = "grey60",
               "Probit" = "grey85")
  ) +
  theme_bw() +
  labs(x = expression(n), y = "RMSE Ratio\n(relative to Probit)") +
  theme(text=element_text(size = 20))
}

bias_plot <- function(results_combined){
  results_combined %>%
    filter(sigma == 7) %>% 
    melt(id.vars = c("data", "rep", "kind", "n", "a")) %>% 
    filter(grepl("bias", variable) & !grepl("bprobit", variable)) %>% 
    mutate(variable = factor(variable, 
                             levels = c("bart_uncali_bias", "nn2_bias", "rf_uncali_bias", "probit_bias"),
                             labels = c("BART", "NN", "RF", "Probit"))) %>% 
    group_by(data, kind, n, variable) %>% 
    summarise(value = mean(value)) %>% 
    ggplot() +
    geom_line(aes(x = n, y = value, colour = factor(variable), linetype = factor(variable), group = factor(variable))) +
    geom_point(aes(x = n, y = value, colour = factor(variable))) +
    geom_hline(aes(yintercept = 0)) +
    facet_grid(kind~data, scales = "free_y") +
    labs(x = "n", y = "Bias (median)", colour = "Model", linetype = "Model") +
    scale_colour_grey("Model", start = 0, end = .8) +
    scale_linetype_manual("Model", values = c("solid", "solid", "solid", "dashed")) +
    theme_bw()
}

rmse_plot <- function(results_combined, a1){
results_combined %>%
    filter(sigma == 7) %>% 
  melt(id.vars = c("data", "rep", "kind", "n", "a")) %>% 
  filter(grepl("mse", variable) & ! grepl("bprobit", variable)) %>% 
  mutate(variable = factor(variable, 
                           levels = c("bart_uncali_mse", "nn2_mse", "rf_uncali_mse", "probit_mse"),
                           labels = c("BART", "NN", "RF", "PROBIT"))) %>% 
  group_by(data, a, n, variable) %>% 
  summarise(value = mean(value)) %>% 
  ggplot() +
  geom_line(aes(x = n, y = value, colour = factor(variable), group = factor(variable))) +
  geom_point(aes(x = n, y = value, colour = factor(variable))) +
  facet_grid(a~data, scales = "free_y") +
  labs(x = "n", y = "RMSE") +
  scale_colour_grey("Model",start = 0, end = .8) +
  theme_bw()
}
