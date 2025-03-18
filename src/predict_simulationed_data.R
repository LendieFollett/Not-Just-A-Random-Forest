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
library(caret)
library(BART)
#library(bartMachine)
library(logistf)
library(Iso)
library(rstanarm)
source("src/functions.R")

numCores <- detectCores() - 1  # Use one less than the total number of cores
cl <- makeCluster(numCores)

registerDoParallel(cl)

a <- c(1,3) #1 = uniform, 3 = beta(3,1) (asymmetric)
sigma <- c(1,2)  # low high

n <- c(1500,1000, 500, 250) + 500
#reserve 500 for the test set

comb <- expand.grid(a,sigma,n) %>% mutate(keep = paste0(Var1, Var2, Var3)) %>% pull(keep)

registerDoParallel(cores = 4)  # Adjust the number of cores as needed

#TO DO
## ADD ERROR VARIABILITY ONTO THE WTP PREDICTIONS FOR THE WTP_MEANS (saved in all)

results <- foreach(comb = comb, .packages = c('BART', 'rstanarm')) %:%
  foreach(reps = 1:100) %dopar% {
    #introduce sparsity into prediction matrix?
    sparsity = FALSE
    #covariates distribution
    a <- as.numeric(substr(comb, 1,1)); b = 1
    #error variance
    sigma <- as.numeric(substr(comb, 2,2))
    sigma_1 <- c(7,15)[sigma]#c(5,10,15)[sigma] #small, medium, large
    sigma_2 <- c(7,15)[sigma]
    
    # Number of observations (sample size)
    n <- as.numeric(substr(comb, 3, 7))
    
    r <- reps
    # Load necessary data and scripts
    # Set seed for reproducibility
    set.seed(r*sigma_1*n + n + sigma_1 + r)
    
    #number of x variables
    nP <- 10
  
    #intercept 
    beta_N0 <- 80 
    beta_linear <- 30
    
    # X_i drawn from U[-1, 1]
    X <- rbeta(n*nP, a, b) %>% matrix(ncol = nP)#runif(n*nP, 0, 1) %>% matrix(ncol = nP)
    
    # Normal WTP_i = beta_N0 + beta_linear * X_i + error term (normally distributed)
    epsilon_Ni <- rnorm(n, mean = 0, sd = sigma_1)
    WTP_normal <- beta_N0 + beta_linear * X[,1] + epsilon_Ni
    
    #Friedman
    WTP_friedman <-  beta_N0 +  2*(10*sin(pi*X[,1]*X[,2]) + 20*(X[,3] - 0.5)^2 + 10*X[,4] + 5*X[,5]) + rnorm(n, 0, sigma_2)
    
    # Step function, normal error
    epsilon_Ni <- rnorm(n, mean = 0, sd = sigma_1)
    WTP_step <- beta_N0 + beta_linear * (X[,1] < 0.5) + epsilon_Ni
    
    # Binary x's only, normal error
    bin_beta <- c(5,10,-15,-25,10, -25, 25) %>% as.matrix()#c(5,10,-15,-25,10,25, 15) %>% as.matrix()#
    epsilon_Ni <- rnorm(n, mean = 0, sd = sigma_1)
    X2 <- apply(X[,1:5], 2, function(x){ifelse(x > 0.5, 1, 0)})
    WTP_bin<- beta_N0 + as.matrix(data.frame(X2, X2[,1]*X2[,2], X2[,1]*X2[,3]))%*%(bin_beta) + epsilon_Ni    
    
    # Create a data frame to store the results
    data <- data.frame(
      WTP_normal = WTP_normal,
      WTP_friedman = WTP_friedman, #the only one that uses X1-X5
      WTP_step = WTP_step,
      WTP_bin = WTP_bin
    )
    
    # Print first few rows
    head(data)
    A <- c(50, 75, 100, 125, 150)
    A_samps <- sample(A, size = nrow(data), replace=TRUE) %>% as.matrix
    
    survey <- data.frame(X = X,
                         A = A_samps,
                         apply(data, 2, function(x){ifelse(x > A_samps, 1, 0)}))
    
    train.idx <- sample(1:nrow(survey), 
                        size = nrow(survey) - 500, #remaining 500 for the test set
                        replace=FALSE)
    
    train <- survey[train.idx,]
    test <-survey[-train.idx,]
    test.wtp <- data[-train.idx,]
    
    # Set up data and test points
    pts <- c(A, c(0, 151))
    epts <- c(min(pts), max(pts))
    
    test$ID <- 1:nrow(test)
    test.y <- test[, paste0("WTP", c("_normal", "_friedman", "_step", "_bin"))]
    
    test2 <- test[rep(1:nrow(test), each = length(pts)),] %>%
      mutate(A = rep(pts, nrow(test)))
    
    test2 <- test2[, c("ID", "A", paste0("X.", 1:10))]
    
    test_ends <- test2 %>% filter(A %in% c(max(pts), min(pts)))
    test_notends <- test2 %>% filter(!A %in% c(max(pts), min(pts)))
    
    all <- list()
    results <- list()
    j = 0
    for (c in c("WTP_normal", "WTP_friedman", "WTP_step", "WTP_bin")){    
      j = j + 1
      if (sparsity == TRUE){
        xa_list <-  c("A", paste0("X.", 1:10))
        x_list <- xa_list[!xa_list == "A"]
        f <- as.formula(paste0(c, "~ A + X.1 + X.2 + X.3 + X.4 + X.5 +X.6 + X.7 + X.8 + X.9 + X.10"))
        f_rf <- as.formula(paste0("as.factor(",c, ")~ A + X.1 + X.2 + X.3 + X.4 + X.5 +X.6 + X.7 + X.8 + X.9 + X.10"))
      } else{ #NO SPARSITY AND (STEP OR NORMAL)
        if (c   %in% c("WTP_step", "WTP_normal")) { # Case when the column is NOT "WTP_friedman"
          xa_list <- c("X.1", "A")
          x_list <- xa_list[!xa_list == "A"]
          f <- as.formula(paste0(c, "~ A + X.1"))
          f_rf <- as.formula(paste0("as.factor(",c, ")~ A + X.1"))
        }else{ #NO SPARSITY AND (FRIEDMAN  OR BINARY)
          xa_list <-  c("A", paste0("X.", 1:5))
          x_list <- xa_list[!xa_list == "A"]
          f <- as.formula(paste0(c, "~ A + X.1 + X.2 + X.3 + X.4 + X.5"))
          f_rf <- as.formula(paste0("as.factor(",c, ")~ A + X.1 + X.2 + X.3 + X.4 + X.5"))
        }   
      }
      
      if (c == "WTP_bin"){
        train<- make_bins(train)
        test<- make_bins(test)
        test_ends <- make_bins(test_ends)
        test_notends <- make_bins(test_notends)
      }
      
      #need to be standardizing test in the same way we standardize the train
      if (!c == "WTP_bin"){
        # Calculate training means and standard deviations for columns starting with "X"
        train_means <- train %>%
          summarise(across(starts_with("X"), mean))
        train_sds <- train %>%
          summarise(across(starts_with("X"), sd))
        
        # Standardize training set
        bprobit_train <- train %>%
          mutate(across(starts_with("X"), ~ (. - train_means[[cur_column()]]) / train_sds[[cur_column()]]))
        
        # Standardize test sets using training means and standard deviations
        #only done for the covariates (not A)
        bprobit_test <- test %>%
          mutate(across(starts_with("X"), ~ (. - train_means[[cur_column()]]) / train_sds[[cur_column()]]))
        
        bprobit_test_ends <- test_ends %>%
          mutate(across(starts_with("X"), ~ (. - train_means[[cur_column()]]) / train_sds[[cur_column()]]))
        
        bprobit_test_notends <- test_notends %>%
          mutate(across(starts_with("X"), ~ (. - train_means[[cur_column()]]) / train_sds[[cur_column()]]))
        
      }else{ #if X's are binary
        bprobit_test <- test
        bprobit_train <- train
        bprobit_test_ends <- test_ends
        bprobit_test_notends <- test_notends
        
      }   
      
      #neural network train/test matrices equal to bprobits (x's standardized)
      n_train <- bprobit_train
      n_test <- bprobit_test
      n_test_ends <- bprobit_test_ends
      n_test_notends <- bprobit_test_notends
      
      #neural network also needs the A standardized
      n_train$A <- (n_train$A - mean(A))/sd(A)
      n_test$A <- (n_test$A - mean(A))/sd(A)
      n_test_ends$A <- (n_test_ends$A - mean(A))/sd(A)
      n_test_notends$A <- (n_test_notends$A - mean(A))/sd(A)
      
      ################################################################
      #FIT BART
      ################################################################
      ndpost = 2000
      b <- pbart(x.train = train[, xa_list],
                 y.train = train[, c],
                 x.test = test_notends[, xa_list],
                 ntree = 100,#cv$num_trees,
                 k = 2,#cv$k,
                 ndpost = ndpost, 
                 nskip = 2000)
      
      # 1. Fit logistic regression on the BART (uncalibrated) probabilities
     # b_probs1 <- b$prob.train.mean
      #calibration_model <- logistf(bprobit_train[,c] ~ b_probs1, family = binomial(link = "logit"),
          #                         plcontrol = logistpl.control(maxit=1000))
      
      #make empty matrix
      #b_probs_cali <- matrix(ncol = nrow(bprobit_test_notends), nrow = ndpost)
      #fill row by row
      #for (i in 1:ndpost){
       # print(i)
        #3. Calibrate probabilities
      #  b_probs_cali[i,] <- predict(calibration_model, data.frame(b_probs1=b$prob.test[i,]),type = "response")
      #}
      
      
      #BART
      #tdat_long2 <- get_wtp2(pred_matrix = b_probs_cali %>% t(),#,b$prob.test %>% t(),#dim = 1000 x 1800
      #                       test_ends = test_ends,
      #                       test_notends=test_notends,
      #                       ndpost = ndpost)
      tdat_long2_uncali <- get_wtp2(pred_matrix = b$prob.test %>% t(),#dim = 1000 x 1800
                             test_ends = test_ends,
                             test_notends=test_notends,
                             ndpost = ndpost)
      ################################################################
      #FIT RF
      ################################################################
      rf <- randomForest(f_rf,
                         data = bprobit_train,
                         ntree = 1000,
                         type = "class") 
      
      rf_probs_uncali <- predict(rf, bprobit_test_notends, type = "prob")[,"1"]
      # 1. Fit logistic regression on the random forest (uncalibrated) probabilities
      rf_probs1 <- predict(rf, bprobit_train, type = "prob")[,"1"]
      calibration_model <- logistf(bprobit_train[,c] ~ rf_probs1, family = binomial(link = "logit"),
                                   plcontrol = logistpl.control(maxit=1000))
      
      #2. Obtain uncalibrated "probabilities" from rf model on the test set
      rf_probs2 <- predict(rf, bprobit_test_notends, type = "prob")[,"1"]
      
      #3. Calibrate probabilities
      rf_probs_cali <- predict(calibration_model, data.frame(rf_probs1=rf_probs2),type = "response")
      tdatrf_long2 <- get_wtp_point(pred_vector = rf_probs_cali, test_ends = bprobit_test_ends,test_notends=bprobit_test_notends)
      
      tdatrf_uncali_long2 <- get_wtp_point(pred_vector = rf_probs_uncali, test_ends = bprobit_test_ends,test_notends=bprobit_test_notends)
      
      ################################################################ 
      #FIT TRADITIONAL PROBIT 
      ################################################################ 
      probit <- glm(f, data = bprobit_train, family = binomial(link = "probit"))
      coefs <- coef(probit)
      WTP_logit <- -coefs["(Intercept)"] / coefs["A"] + as.matrix(bprobit_test[, x_list])%*%as.matrix(-coefs[x_list] / coefs["A"])
      
      ################################################################  
      #FIT BAYESIAN PROBIT 
      ################################################################ 
      bprobit <- stan_glm(f, 
                          data = bprobit_train , 
                          family = binomial(link = "probit"), 
                          prior = normal(0, 1), 
                          prior_intercept = normal(0, 1), 
                          chains = 1, iter = 1000,
                          init = "0")
      
      bcoefs <- as.data.frame(bprobit) %>% apply(2, mean)
      WTP_bprobit <- -bcoefs["(Intercept)"] / bcoefs["A"] + as.matrix(bprobit_test[, x_list])%*%as.matrix(-bcoefs[x_list] / bcoefs["A"]) 
      
  
      WTP_bprobit2 <- get_wtp_point(pred_vector = predict(bprobit, newdata=bprobit_test_notends[colnames(bprobit_train)[1:11]], type="response"),
                                    test_ends = bprobit_test_ends,test_notends=bprobit_test_notends)
      
      #get_wtp_point gives effectively the same solution as the standard probit formula
      #what about get_wtp(pred_matrix = posterior_epred(bprobit,  test_notends[,-1] ) %>% t())
     #WTP_bprobit3 <- get_wtp2(pred_matrix= posterior_epred(bprobit,  test_notends[,-1] ) %>% t(), 
      #         test_ends,test_notends, ndpost=500)
      #get_wtp2 and get_wtp_point result in identical numbers
      ################################################################
      #FIT NEURAL NETWORK
      ################################################################ 
      # Define the tuning grid
      tune_grid <- expand.grid(size = c(2, 4, 6, 8),   # Number of hidden units
                               decay = c(0.01, 0.1, 0.5)) # Regularization parameter
      
      # Train the model using caret
      set.seed(123)
      nnet_model <- train(n_train[,xa_list], as.factor(n_train[,c]),
                          method = "nnet",
                          tuneGrid = tune_grid,
                          trControl = trainControl(method = "cv", number = 5),
                          trace = FALSE)
      
      #ANN
      tdatn2_long2 <- get_wtp_point(pred_vector = predict(nnet_model, n_test_notends, type = "prob")[,"1"], test_ends = bprobit_test_ends,test_notends=bprobit_test_notends)
      
      ################################################################
      
      if(r <=10){
      all[[j]] <- data.frame(n = n - 500,
                              data = gsub("WTP_", "", c), 
                             a = ifelse(a == 1, "symmetric", "asymmetric"),
                              sigma = sigma_1,
                             sparsity = sparsity,
                             rep = r,
                              true = test.wtp[, c],
                              #bart_q = tdat_long2$wtp_q,
                              bart_uncali_q = tdat_long2_uncali$wtp_q,
                              nn2_q = tdatn2_long2$wtp_q,
                              rf = tdatrf_long2$wtp_q,
                              rf_uncali = tdatrf_uncali_long2$wtp_q,
                              probit = WTP_logit,
                              bprobit = WTP_bprobit)
      }
      #one row per rep
      results[[j]] <- data.frame(n = n - 500,
                                 data = gsub("WTP_", "", c),
                                 sigma = sigma_1,
                                 sparsity = sparsity,
                                 a = ifelse(a == 1, "symmetric", "asymmetric"),
                                 rep = r,
                                 
                                 #average individual mse
                                 #bart_mse = mean((tdat_long2$wtp_q - test.wtp[, c])^2)^0.5,
                                 bart_uncali_mse = mean((tdat_long2_uncali$wtp_q - test.wtp[, c])^2)^0.5,
                                 nn2_mse = mean((tdatn2_long2$wtp_q - test.wtp[, c])^2)^0.5,
                                 rf_mse = mean((tdatrf_long2$wtp_q - test.wtp[, c])^2)^0.5,
                                 rf_uncali_mse = mean((tdatrf_uncali_long2$wtp_q - test.wtp[, c])^2)^0.5,
                                 probit_mse = mean((WTP_logit - test.wtp[, c])^2)^0.5,
                                 bprobit_mse = mean((WTP_bprobit - test.wtp[, c])^2)^0.5,
                                 
                                 #median of the test sample predicted WTP minus median true WTP
                                 #bart_bias = median(tdat_long2$wtp_q) - median(test.wtp[, c]),
                                 bart_uncali_bias = median(tdat_long2_uncali$wtp_q) - median(test.wtp[, c]),
                                 nn2_bias = median(tdatn2_long2$wtp_q) - median(test.wtp[, c]),
                                 rf_bias = median(tdatrf_long2$wtp_q) - median(test.wtp[, c]),
                                 rf_uncali_bias = median(tdatrf_uncali_long2$wtp_q) - median(test.wtp[, c]),
                                 probit_bias = median(WTP_logit) - median(test.wtp[, c]),
                                 bprobit_bias = median(WTP_bprobit) - median(test.wtp[, c]),
                                 
                                 bart_uncali_bias_mn = mean(tdat_long2_uncali$wtp_q) - mean(test.wtp[, c]),
                                 nn2_bias_mn = mean(tdatn2_long2$wtp_q) - mean(test.wtp[, c]),
                                 rf_bias_mn = mean(tdatrf_long2$wtp_q) - mean(test.wtp[, c]),
                                 rf_uncali_bias_mn = mean(tdatrf_uncali_long2$wtp_q) - mean(test.wtp[, c]),
                                 probit_bias_mn = mean(WTP_logit) - mean(test.wtp[, c]),
                                 bprobit_bias_mn = mean(WTP_bprobit) - mean(test.wtp[, c]),
                                 
                                 mean_true = mean(test.wtp[,c]),
                                 mean_bart = mean(tdat_long2_uncali$wtp_q),
                                 mean_nn = mean(tdatn2_long2$wtp_q),
                                 mean_rf = mean(tdatrf_long2$wtp_q),
                                 mean_rf_uncali = mean(tdatrf_uncali_long2$wtp_q),
                                 mean_probit = mean(WTP_logit),
                                 mean_bprobit = mean(WTP_bprobit)
                                 )
      
    }
    all <- do.call(rbind, all)
    results <- do.call(rbind, results)
    
    list( results = results,all = all)
  }



all_combined <- do.call(rbind, lapply(results, function(x) do.call(rbind, lapply(x, function(y) y$all))))
results_combined <- do.call(rbind, lapply(results, function(x) do.call(rbind, lapply(x, function(y) y$results))))


sparsity = FALSE

saveRDS(all_combined, paste0("all_combined_",sparsity,".RDS"))
saveRDS(results_combined, paste0("results_combined_",sparsity,".RDS"))

#results_combinedTRUE <- readRDS( paste0("results_combined_",TRUE,".RDS")) %>% mutate(kind = "Sparse")
#results_combinedFALSE <- readRDS( paste0("results_combined_",FALSE,".RDS")) %>% mutate(kind = "Not Sparse")

