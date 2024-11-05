rm(list = ls())
library(pROC)
library(tidyverse)
library(foreach)
library(nnet)
library(foreach)
library(doParallel)
library(tidyr)
library(reshape2)
source("functions.R")

numCores <- detectCores() - 1  # Use one less than the total number of cores
cl <- makeCluster(numCores)
registerDoParallel(cl)

datasets <- c("WTP_normal" , "WTP_friedman", "WTP_step") 

registerDoParallel(cores = 4)  # Adjust the number of cores as needed

results <- foreach(c = datasets, .packages = c('BART', 'rstanarm')) %:%
  foreach(reps = 1:10) %dopar% {
    #introduce sparsity into prediction matrix?
    sparsity = FALSE
    r <- reps
    # Load necessary data and scripts
    # Set seed for reproducibility
    set.seed(1234 + r)
    
    # Number of observations
    n <- 1000
    
    #number of x variables
    nP <- 10
    
    # standard deviation
    sigma_1 <- 10
    sigma_2 <- 1
    #intercept 
    beta_N0 <- 100 
    beta_linear <- 25
    
    # X_i drawn from U[-1, 1]
    X <- runif(n*nP, 0, 1) %>% matrix(ncol = nP)
    
    # Normal WTP_i = beta_N0 + beta_linear * X_i + error term (normally distributed)
    epsilon_Ni <- rnorm(n, mean = 0, sd = sigma_1)
    WTP_normal <- beta_N0 + beta_linear * X[,1] + epsilon_Ni
    
    #Friedman
    WTP_friedman <-  30 +  5*(10*sin(pi*X[,1]*X[,2]) + 20*(X[,3] - 0.5)^2 + 10*X[,4] + 5*X[,5]) + rnorm(n, 0, sigma_2)
    
    # Step function, normal error
    epsilon_Ni <- rnorm(n, mean = 0, sd = sigma_1)
    WTP_step <- beta_N0 + beta_linear * (X[,1] < 0.5) + epsilon_Ni
    
    
    # Create a data frame to store the results
    data <- data.frame(
      WTP_normal = WTP_normal,
      WTP_friedman = WTP_friedman, #the only one that uses X1-X5
      WTP_step = WTP_step
    )
    
    # Print first few rows
    head(data)
    A <- c(25, 50, 75, 125, 150)
    A_samps <- sample(A, size = nrow(data), replace=TRUE) %>% as.matrix
    
    survey <- data.frame(X = X,
                         A = A_samps,
                         apply(data, 2, function(x){ifelse(x > A_samps, 1, 0)}))
    
    train.idx <- sample(1:nrow(survey), size = .7*nrow(survey), replace=FALSE)
    
    train <- survey[train.idx,]
    test <-survey[-train.idx,]
    test.wtp <- data[-train.idx,]

    # Set up data and test points
    pts <- c(A, c(0, 151))
    epts <- c(min(pts), max(pts))
    
    test$ID <- 1:nrow(test)
    test.y <- test[, paste0("WTP", c("_normal", "_friedman", "_step"))]
    
    test2 <- test[rep(1:nrow(test), each = length(pts)),] %>%
      mutate(A = rep(pts, nrow(test)))
    
    test2 <- test2[, c("ID", "A", paste0("X.", 1:10))]
    
    test_ends <- test2 %>% filter(A %in% c(max(pts), min(pts)))
    test_notends <- test2 %>% filter(!A %in% c(max(pts), min(pts)))
    
 if (sparsity == TRUE){
   if (c != "WTP_friedman") { # Case when the column is NOT "WTP_friedman"
     xa_list <- c("X", "A")
     x_list <- xa_list[!xa_list == "A"]
     f <- as.formula(paste0(c, "~ A + X.1 + X.2 + X.3 + X.4 + X.5 +X.6 + X.7 + X.8 + X.9 + X.10"))
   }else{ #Friedman
     xa_list <-  c("A", paste0("X.", 1:10))
     x_list <- xa_list[!xa_list == "A"]
     f <- as.formula(paste0(c, "~ A + X.1 + X.2 + X.3 + X.4 + X.5 +X.6 + X.7 + X.8 + X.9 + X.10"))
   }
 } else{
   if (c != "WTP_friedman") { # Case when the column is NOT "WTP_friedman"
     xa_list <- c("X", "A")
     x_list <- xa_list[!xa_list == "A"]
     f <- as.formula(paste0(c, "~ A + X.1"))
   }else{ #Friedman
     xa_list <-  c("A", paste0("X.", 1:5))
     x_list <- xa_list[!xa_list == "A"]
     f <- as.formula(paste0(c, "~ A + X.1 + X.2 + X.3 + X.4 + X.5"))
   }   
 } 

      
    #FIT BART
      mb_normal <- pbart(x.train = train[, xa_list],
                         y.train = train[, c],
                         x.test = test_notends[, xa_list],
                         ntree = 400,
                         ndpost = 1000, nskip = 3000)
      

      bprobit_train <- train %>%  mutate(across(starts_with("X"), function(x){(x - mean(x))/sd(x)}))
      bprobit_test <- test %>%  mutate(across(starts_with("X"), function(x){(x - mean(x))/sd(x)}))
      bprobit_test_ends <- test_ends%>%  mutate(across(starts_with("X"), function(x){(x - mean(x))/sd(x)}))
      bprobit_test_notends <- test_notends%>%  mutate(across(starts_with("X"), function(x){(x - mean(x))/sd(x)}))
      
     #FIT TRADITIONAL PROBIT 
      probit <- glm(f, data = bprobit_train, family = binomial(link = "probit"))
     #FIT BAYESIAN PROBIT 
      bprobit <- stan_glm(f, 
                             data = bprobit_train , 
                             family = binomial(link = "probit"), 
                             prior = normal(0, 1), 
                             prior_intercept = normal(0, 1), 
                             chains = 1, iter = 2000,
                          init = "0")
      
    #FIT NEURAL NETWORK
      nn_model2 <- nnet(f, data = bprobit_train, size = 2, decay = 0.01, maxit = 200)
      nn_model3 <- nnet(f, data = bprobit_train, size = 3, decay = 0.01, maxit = 200)
      nn_model4 <- nnet(f, data = bprobit_train, size = 4, decay = 0.01, maxit = 200)
      nn_model5 <- nnet(f, data = bprobit_train, size = 5, decay = 0.01, maxit = 200)
      
    #COLLECT PREDICTIONS FROM Machine Learning MODELS 
      #BART
      tdat_long2 <- get_wtp2(pred_matrix = (1 - mb_normal$prob.test) %>% t(), test_ends = test_ends,test_notends=test_notends)
      #ANN
      tdatn2_long2 <- get_wtp_point(pred_vector = 1-predict(nn_model2, bprobit_test_notends, type = "raw"), test_ends = bprobit_test_ends,test_notends=bprobit_test_notends)
      tdatn3_long2 <- get_wtp_point(pred_vector = 1-predict(nn_model3, bprobit_test_notends, type = "raw"), test_ends = bprobit_test_ends,test_notends=bprobit_test_notends)
      tdatn4_long2 <- get_wtp_point(pred_vector = 1-predict(nn_model4, bprobit_test_notends, type = "raw"), test_ends = bprobit_test_ends,test_notends=bprobit_test_notends)
      tdatn5_long2 <- get_wtp_point(pred_vector = 1-predict(nn_model5, bprobit_test_notends, type = "raw"), test_ends = bprobit_test_ends,test_notends=bprobit_test_notends)
      
      coefs <- coef(probit)
      WTP_logit <- -coefs["(Intercept)"] / coefs["A"] + as.matrix(bprobit_test[, x_list])%*%as.matrix(-coefs[x_list] / coefs["A"])
      
      bcoefs <- as.data.frame(bprobit) %>% apply(2, mean)
      WTP_bprobit <- -bcoefs["(Intercept)"] / bcoefs["A"] + as.matrix(bprobit_test[, x_list])%*%as.matrix(-bcoefs[x_list] / bcoefs["A"]) 
      
      
      all <- data.frame(data = test.wtp[, c],
                        data_name = gsub("WTP_", "", c),
                        bart_q = tdat_long2$wtp_q,
                        nn2_q = tdatn2_long2$wtp_q,
                        nn3_q = tdatn3_long2$wtp_q,
                        nn4_q = tdatn4_long2$wtp_q,
                        nn5_q = tdatn5_long2$wtp_q,
                        probit = WTP_logit,
                        bprobit = WTP_bprobit)
      
      results <- data.frame(bart_q = mean((tdat_long2$wtp_q - test.wtp[, c])^2)^0.5,
                            nn2_q = mean((tdatn2_long2$wtp_q - test.wtp[, c])^2)^0.5,
                            nn3_q = mean((tdatn3_long2$wtp_q - test.wtp[, c])^2)^0.5,
                            nn4_q = mean((tdatn4_long2$wtp_q - test.wtp[, c])^2)^0.5,
                            nn5_q = mean((tdatn5_long2$wtp_q - test.wtp[, c])^2)^0.5,
                            probit = mean((WTP_logit - test.wtp[, c])^2)^0.5,
                            bprobit = mean((WTP_bprobit - test.wtp[, c])^2)^0.5,
                            data = gsub("WTP_", "", c),
                            rep = r)
    
    

    list(all = all, results = results)
  }


all_combined <- do.call(rbind, lapply(results, function(x) do.call(rbind, lapply(x, function(y) y$all))))
results_combined <- do.call(rbind, lapply(results, function(x) do.call(rbind, lapply(x, function(y) y$results))))


results_combined %>% group_by(data) %>% 
  summarise(#bart_t = mean(bart_t),
            #lr_t = mean(lr_t),
            bart_q = mean(bart_q)/mean(probit),
            nn_q = mean(nn_q)/mean(probit),
           # lr_q = mean(lr_q)/mean(probit),
            #probit = mean(probit)/mean(probit),
            bprobit = mean(bprobit)/mean(probit)
            ) %>% 
  xtable()



results_combined %>% 
  group_by(data) %>% 
  summarise(
    #bart_q_min = min(bart_q / probit, na.rm = TRUE),
    #bart_q_q1 = quantile(bart_q / probit, 0.25, na.rm = TRUE),
    #bart_q_median = median(bart_q / probit, na.rm = TRUE),
    bart_q_mean = mean(bart_q / probit, na.rm = TRUE),
    #bart_q_q3 = quantile(bart_q / probit, 0.75, na.rm = TRUE),
    #bart_q_max = max(bart_q / probit, na.rm = TRUE),
    
    #nn_q_min = min(nn_q / probit, na.rm = TRUE),
    #nn_q_q1 = quantile(nn_q / probit, 0.25, na.rm = TRUE),
    #nn_q_median = median(nn_q / probit, na.rm = TRUE),
    nn_q_mean = mean(nn_q / probit, na.rm = TRUE),
    #nn_q_q3 = quantile(nn_q / probit, 0.75, na.rm = TRUE),
    #nn_q_max = max(nn_q / probit, na.rm = TRUE),
    
    #bprobit_min = min(bprobit / probit, na.rm = TRUE),
    #bprobit_q1 = quantile(bprobit / probit, 0.25, na.rm = TRUE),
    #bprobit_median = median(bprobit / probit, na.rm = TRUE),
    bprobit_mean = mean(bprobit / probit, na.rm = TRUE),
    #bprobit_q3 = quantile(bprobit / probit, 0.75, na.rm = TRUE),
    #bprobit_max = max(bprobit / probit, na.rm = TRUE)
  )

results_combined %>% 
  group_by(data) %>% 
  summarise(diff = t.test(bart_q, probit, paired = TRUE)$estimate,
            statistic = t.test(bart_q, probit, paired = TRUE)$statistic,
            p.value = t.test(bart_q, probit, paired = TRUE)$p.value
  )%>% 
  xtable(digits = c(1,10,2,2,-1))



results_combined %>%
  melt(id.vars = c("data", "rep")) %>% 
  #filter(!grepl("bp", variable)) %>% 
  ggplot() +
  geom_boxplot(aes(x = variable, y = value, colour = variable)) +
  facet_wrap(~data, scales = "free_y") +
  labs(x = "Model", y = "RMSE")


ggplot() + 
  geom_point(aes(y=probit, x=data), 
             data = filter(all_combined, data_name == "friedman")) + 
  geom_abline(aes(slope = 1, intercept = 0)) +
  labs(x = "True WTP", y = "Predicted WTP")

###########

ggplot() + 
  geom_point(aes(y=survey$WTP_friedman, x=tdat_long2$wtp_q)) + 
  geom_abline(aes(slope = 1, intercept = 0)) +
  labs(x = "True WTP", y = "Predicted WTP")

