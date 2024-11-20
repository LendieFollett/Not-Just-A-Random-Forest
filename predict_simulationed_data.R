rm(list = ls())
library(pROC)
library(tidyverse)
library(foreach)
library(nnet)
library(foreach)
library(doParallel)
library(tidyr)
library(reshape2)
library(randomForest)
source("functions.R")

numCores <- detectCores() - 1  # Use one less than the total number of cores
cl <- makeCluster(numCores)
registerDoParallel(cl)

datasets <- c("WTP_normal" , "WTP_friedman", "WTP_step") 

registerDoParallel(cores = 4)  # Adjust the number of cores as needed

results <- foreach(c = datasets, .packages = c('BART', 'rstanarm')) %:%
  foreach(reps = 1:10) %dopar% {
    #introduce sparsity into prediction matrix?
    sparsity = TRUE
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
    beta_N0 <- 90 
    beta_linear <- 30
    
    # X_i drawn from U[-1, 1]
    X <- runif(n*nP, 0, 1) %>% matrix(ncol = nP)
    
    # Normal WTP_i = beta_N0 + beta_linear * X_i + error term (normally distributed)
    epsilon_Ni <- rnorm(n, mean = 0, sd = sigma_1)
    WTP_normal <- beta_N0 + beta_linear * X[,1] + epsilon_Ni
    
    #Friedman
    WTP_friedman <-  10 +  5*(10*sin(pi*X[,1]*X[,2]) + 20*(X[,3] - 0.5)^2 + 10*X[,4] + 5*X[,5]) + rnorm(n, 0, sigma_2)
    
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
    A <- c(25, 50, 75, 100, 125, 150)
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
     xa_list <- c("X.1", "A")
     x_list <- xa_list[!xa_list == "A"]
     f <- as.formula(paste0(c, "~ A + X.1 + X.2 + X.3 + X.4 + X.5 +X.6 + X.7 + X.8 + X.9 + X.10"))
     f_rf <- as.formula(paste0("as.factor(",c, ")~ A + X.1 + X.2 + X.3 + X.4 + X.5 +X.6 + X.7 + X.8 + X.9 + X.10"))
   }else{ #Friedman
     xa_list <-  c("A", paste0("X.", 1:10))
     x_list <- xa_list[!xa_list == "A"]
     f <- as.formula(paste0(c, "~ A + X.1 + X.2 + X.3 + X.4 + X.5 +X.6 + X.7 + X.8 + X.9 + X.10"))
     f_rf <- as.formula(paste0("as.factor(",c, ")~ A + X.1 + X.2 + X.3 + X.4 + X.5 +X.6 + X.7 + X.8 + X.9 + X.10"))
   }
 } else{
   if (c != "WTP_friedman") { # Case when the column is NOT "WTP_friedman"
     xa_list <- c("X.1", "A")
     x_list <- xa_list[!xa_list == "A"]
     f <- as.formula(paste0(c, "~ A + X.1"))
     f_rf <- as.formula(paste0("as.factor(",c, ")~ A + X.1"))
   }else{ #Friedman
     xa_list <-  c("A", paste0("X.", 1:5))
     x_list <- xa_list[!xa_list == "A"]
     f <- as.formula(paste0(c, "~ A + X.1 + X.2 + X.3 + X.4 + X.5"))
     f_rf <- as.formula(paste0("as.factor(",c, ")~ A + X.1 + X.2 + X.3 + X.4 + X.5"))
   }   
 } 

      
    #FIT BART
      mb_normal <- pbart(x.train = train[, xa_list],
                         y.train = train[, c],
                         x.test = test_notends[, xa_list],
                         ntree = 100,
                         ndpost = 1000, nskip = 3000)
      
      

      bprobit_train <- train %>%  mutate(across(starts_with("X"), function(x){(x - mean(x))/sd(x)}))
      bprobit_test <- test %>%  mutate(across(starts_with("X"), function(x){(x - mean(x))/sd(x)}))
      bprobit_test_ends <- test_ends%>%  mutate(across(starts_with("X"), function(x){(x - mean(x))/sd(x)}))
      bprobit_test_notends <- test_notends%>%  mutate(across(starts_with("X"), function(x){(x - mean(x))/sd(x)}))
     
      
      n_train <- train %>%  mutate(across(starts_with("X"), function(x){(x - mean(x))/sd(x)}))
      n_train$A <- (n_train$A - mean(A))/sd(A)
      n_test <- test %>%  mutate(across(starts_with("X"), function(x){(x - mean(x))/sd(x)}))
      n_test$A <- (n_test$A - mean(A))/sd(A)
      n_test_ends <- test_ends%>%  mutate(across(starts_with("X"), function(x){(x - mean(x))/sd(x)}))
      n_test_ends$A <- (n_test_ends$A - mean(A))/sd(A)
      n_test_notends <- test_notends%>%  mutate(across(starts_with("X"), function(x){(x - mean(x))/sd(x)}))
      n_test_notends$A <- (n_test_notends$A - mean(A))/sd(A)
      #FIT RF
      rf <- randomForest(f_rf,
                         data = bprobit_train,
                         ntree = 1000,
                         type = "class") 
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
      nn_model2 <- nnet(f, data = n_train, size = 2, maxit = 1000)
      nn_model3 <- nnet(f, data = n_train, size = 3, maxit = 1000)
      nn_model4 <- nnet(f, data = n_train, size = 4, maxit = 1000)
      nn_model5 <- nnet(f, data = n_train, size = 5, maxit = 1000)
      nn_model6 <- nnet(f, data = n_train, size = 6, maxit = 1000)
      nn_model7 <- nnet(f, data = n_train, size = 7, maxit = 1000)
      nn_model8 <- nnet(f, data = n_train, size = 8, maxit = 1000)
      nn_model9 <- nnet(f, data = n_train, size = 9, maxit = 1000)
      
    #COLLECT PREDICTIONS FROM Machine Learning MODELS 
      #BART
      tdat_long2 <- get_wtp2(pred_matrix = mb_normal$prob.test %>% t(), test_ends = test_ends,test_notends=test_notends)
      
      #ANN
      tdatn2_long2 <- get_wtp_point(pred_vector = predict(nn_model2, n_test_notends, type = "raw"), test_ends = bprobit_test_ends,test_notends=bprobit_test_notends)
      tdatn3_long2 <- get_wtp_point(pred_vector = predict(nn_model3, n_test_notends, type = "raw"), test_ends = bprobit_test_ends,test_notends=bprobit_test_notends)
      tdatn4_long2 <- get_wtp_point(pred_vector = predict(nn_model4, n_test_notends, type = "raw"), test_ends = bprobit_test_ends,test_notends=bprobit_test_notends)
      tdatn5_long2 <- get_wtp_point(pred_vector = predict(nn_model5, n_test_notends, type = "raw"), test_ends = bprobit_test_ends,test_notends=bprobit_test_notends)
      tdatn6_long2 <- get_wtp_point(pred_vector = predict(nn_model6, n_test_notends, type = "raw"), test_ends = bprobit_test_ends,test_notends=bprobit_test_notends)
      tdatn7_long2 <- get_wtp_point(pred_vector = predict(nn_model7, n_test_notends, type = "raw"), test_ends = bprobit_test_ends,test_notends=bprobit_test_notends)
      tdatn8_long2 <- get_wtp_point(pred_vector = predict(nn_model8, n_test_notends, type = "raw"), test_ends = bprobit_test_ends,test_notends=bprobit_test_notends)
      tdatn9_long2 <- get_wtp_point(pred_vector = predict(nn_model9, n_test_notends, type = "raw"), test_ends = bprobit_test_ends,test_notends=bprobit_test_notends)
      
      #RANDOM FOREST
      tdatrf_long2 <- get_wtp_point(pred_vector = predict(rf, bprobit_test_notends, type = "prob")[,"1"], test_ends = bprobit_test_ends,test_notends=bprobit_test_notends)
      
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
                        rf = tdatrf_long2$wtp_q,
                        probit = WTP_logit,
                        bprobit = WTP_bprobit)
      
      results <- data.frame(bart_q = mean((tdat_long2$wtp_q - test.wtp[, c])^2)^0.5,
                            nn2_q = mean((tdatn2_long2$wtp_q - test.wtp[, c])^2)^0.5,
                            nn3_q = mean((tdatn3_long2$wtp_q - test.wtp[, c])^2)^0.5,
                            nn4_q = mean((tdatn4_long2$wtp_q - test.wtp[, c])^2)^0.5,
                            nn5_q = mean((tdatn5_long2$wtp_q - test.wtp[, c])^2)^0.5,
                            nn6_q = mean((tdatn6_long2$wtp_q - test.wtp[, c])^2)^0.5,
                            nn7_q = mean((tdatn7_long2$wtp_q - test.wtp[, c])^2)^0.5,
                            nn8_q = mean((tdatn8_long2$wtp_q - test.wtp[, c])^2)^0.5,
                            nn9_q = mean((tdatn9_long2$wtp_q - test.wtp[, c])^2)^0.5,
                            rf_q = mean((tdatrf_long2$wtp_q - test.wtp[, c])^2)^0.5,
                            probit = mean((WTP_logit - test.wtp[, c])^2)^0.5,
                            bprobit = mean((WTP_bprobit - test.wtp[, c])^2)^0.5,
                            data = gsub("WTP_", "", c),
                            rep = r)
    
    

    list(all = all, results = results)
  }


all_combined <- do.call(rbind, lapply(results, function(x) do.call(rbind, lapply(x, function(y) y$all))))
results_combined <- do.call(rbind, lapply(results, function(x) do.call(rbind, lapply(x, function(y) y$results))))

sparsity = TRUE
saveRDS(all_combined, paste0("all_combined_",sparsity,".RDS"))
saveRDS(results_combined, paste0("results_combined_",sparsity,".RDS"))



results_combinedTRUE <- readRDS( paste0("results_combined_",TRUE,".RDS")) %>% mutate(kind = "Sparse")
results_combinedFALSE <- readRDS( paste0("results_combined_",FALSE,".RDS")) %>% mutate(kind = "Not Sparse")

rbind(results_combinedTRUE,results_combinedFALSE) %>% 
  group_by(kind, data) %>% 
  summarise(bart_q = mean(bart_q)/mean(probit),
            nn2_q = mean(nn2_q)/mean(probit),
            nn3_q = mean(nn3_q)/mean(probit),
            nn4_q = mean(nn4_q)/mean(probit),
            nn5_q = mean(nn5_q)/mean(probit),
            nn6_q = mean(nn6_q)/mean(probit),
            nn7_q = mean(nn7_q)/mean(probit),
            nn8_q = mean(nn8_q)/mean(probit),
            nn9_q = mean(nn9_q)/mean(probit),
            rf = mean(rf_q)/mean(probit),
            bprobit = mean(bprobit)/mean(probit)
            ) %>% 
  ungroup() %>% 
  rowwise() %>% t() %>% 
  #mutate(across(
  #  bart_q:bprobit,
  #  ~ ifelse(. == min(c_across(bart_q:bprobit)), paste0("\\textbf{", . , "}"), .)
  #)) %>% 
  xtable(escape = FALSE)



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
  facet_wrap(~data, scales = "free_y", ncol = 1) +
  labs(x = "Model", y = "RMSE")


ggplot() + 
  geom_point(aes(y=nn5_q, x=data), 
             data = filter(all_combined, data_name == "normal")) + 
  geom_abline(aes(slope = 1, intercept = 0)) +
  labs(x = "True WTP", y = "Predicted WTP")

###########

ggplot() + 
  geom_point(aes(y=survey$WTP_friedman, x=tdat_long2$wtp_q)) + 
  geom_abline(aes(slope = 1, intercept = 0)) +
  labs(x = "True WTP", y = "Predicted WTP")

