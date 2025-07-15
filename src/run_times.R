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
library(GGally)
source("src/functions.R")

numCores <- detectCores() - 1  # Use one less than the total number of cores
cl <- makeCluster(numCores)

registerDoParallel(cl)

a <- c(1) #1 = uniform, 3 = beta(3,1) (asymmetric)
sigma <- c(1,2)  # low high

n <- c(1500, 250) + 500
#reserve 500 for the test set

comb <- expand.grid(a,sigma,n) %>% mutate(keep = paste0(Var1, Var2, Var3)) %>% pull(keep)

registerDoParallel(cores = 4)  # Adjust the number of cores as needed

#TO DO
## ADD ERROR VARIABILITY ONTO THE WTP PREDICTIONS FOR THE WTP_MEANS (saved in all)

results <- foreach(comb = comb, .packages = c('BART', 'rstanarm')) %:%
  foreach(reps = 1) %dopar% {
    #introduce sparsity into prediction matrix?
    sparsity = TRUE
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
    mean_normal <- beta_N0 + beta_linear * X[,1]
    WTP_normal <- mean_normal + epsilon_Ni
    
    #Friedman
    mean_friedman <- beta_N0 +  2*(10*sin(pi*X[,1]*X[,2]) + 20*(X[,3] - 0.5)^2 + 10*X[,4] + 5*X[,5])
    WTP_friedman <-  mean_friedman + rnorm(n, 0, sigma_2)
    
    # Step function, normal error
    epsilon_Ni <- rnorm(n, mean = 0, sd = sigma_1)
    mean_step <- beta_N0 + beta_linear * (X[,1] < 0.5) 
    WTP_step <- mean_step+ epsilon_Ni
    
    # Binary x's only, normal error
    bin_beta <- c(5,10,-15,-25,10, -25, 25) %>% as.matrix()#c(5,10,-15,-25,10,25, 15) %>% as.matrix()#
    epsilon_Ni <- rnorm(n, mean = 0, sd = sigma_1)
    X2 <- apply(X[,1:5], 2, function(x){ifelse(x > 0.5, 1, 0)})
    mean_bin = beta_N0 + as.matrix(data.frame(X2, X2[,1]*X2[,2], X2[,1]*X2[,3]))%*%(bin_beta)
    WTP_bin<- mean_bin + epsilon_Ni    
    
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
    
    #P(yes) = P(Y > A) = P(N(mu, \sigma^2) > A) = 1-pnorm(A, mean = mean_?, sd = sigma_1)
    true_probs <- data.frame(WTP_friedman=1-pnorm(A_samps, mean = mean_friedman, sd = sigma_2),
                             WTP_step = 1-pnorm(A_samps, mean = mean_step, sd = sigma_1),
                             WTP_normal = 1-pnorm(A_samps, mean = mean_normal, sd = sigma_1),
                             WTP_bin = 1-pnorm(A_samps, mean = mean_bin, sd = sigma_1))
    
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
      bart1 <- Sys.time()
      b <- pbart(x.train = train[, xa_list],
                 y.train = train[, c],
                 x.test = test[, xa_list],
                 ntree = 100,#cv$num_trees,
                 k = 2,#cv$k,
                 ndpost = ndpost, 
                 nskip = 2000)
      
      bart2 <- Sys.time()
      bart_time <- (bart2 - bart1)
      bart_probs <- b$prob.test  %>% colMeans
      
      ################################################################
      #FIT RF
      ################################################################
      rf1 <- Sys.time()
      rf <- randomForest(f_rf,
                         data = bprobit_train,
                         ntree = 1000,
                         type = "class") 
      rf2 <- Sys.time()
      rf_time <- (rf2 - rf1)
      
      rf_probs <- predict(rf, bprobit_test, type = "prob")[,"1"]

      ################################################################ 
      #FIT TRADITIONAL PROBIT 
      ################################################################ 
      probit1 <- Sys.time()
      probit <- glm(f, data = bprobit_train, family = binomial(link = "probit"))
      probit2 <- Sys.time()
      probit_time <- (probit2 - probit1) 
      
      probit_probs <- predict(probit, bprobit_test, type = "response")
      ################################################################  
      #FIT BAYESIAN PROBIT 
      ################################################################ 
      bprobit1 <- Sys.time()
      bprobit <- stan_glm(f, 
                          data = bprobit_train , 
                          family = binomial(link = "probit"), 
                          prior = normal(0, 1), 
                          prior_intercept = normal(0, 1), 
                          chains = 1, iter = 2000,
                          init = "0")
      bprobit2 <- Sys.time()
      bprobit_time <- bprobit2 - bprobit1
      bprobit_probs <- posterior_epred(bprobit, newdata = bprobit_test) %>% colMeans
 
      ################################################################
      #FIT NEURAL NETWORK
      ################################################################ 
      nn1 <- Sys.time()
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
      nn2 <- Sys.time()
      nn_time <- nn2-nn1
      nn_probs <- predict(nnet_model, n_test, type = "prob")[,"1"]

      ################################################################
      
#save times
      results[[j]] <- data.frame(n = n - 500,
                                 a = ifelse(a == 1, "symmetric", "asymmetric"),
                                 sigma = sigma_1,
                                 sparsity = sparsity,
                             data = gsub("WTP_", "", c),
                             bart = bart_time,
                             rf = rf_time,
                             nn = nn_time,
                             probit = probit_time,
                             bprobit = bprobit_time )

      #500 rows per rep
      
      all[[j]] <- rbind(data.frame(n = n - 500,
                                   a = ifelse(a == 1, "symmetric", "asymmetric"),
                                   sigma = sigma_1,
                                   sparsity = sparsity,
                                 data = gsub("WTP_", "", c),
                                 true = true_probs[-train.idx,c],
                                 bart = bart_probs,
                                 rf = rf_probs,
                                 nn = nn_probs,
                                 probit = probit_probs,
                                 bprobit = bprobit_probs)
                        )
      
    }
    all <- do.call(rbind, all)
    results <- do.call(rbind, results)
    
    list( results = results,all = all)
  }



results_combined <- do.call(rbind, lapply(results, function(x) do.call(rbind, lapply(x, function(y) y$results))))
all_combined <- do.call(rbind, lapply(results, function(x) do.call(rbind, lapply(x, function(y) y$all))))

#Illustration of stage 1 model performances - comparison of predicted probabilities vs true probabilities

library(scales)

#friedman
all_combined %>% 
  filter(n == 1500 & sigma == 15 & data == "friedman") %>% 
  select(true, bart, rf, nn, probit, bprobit) %>% 
ggpairs(lower = list(continuous = wrap("points", alpha = 0.2))) + 
  theme_bw() + 
  theme(text = element_text(size = 20)) +
  scale_x_continuous(breaks = c(0, .5, 1))
ggsave("corr_friedman.pdf", width = 10, height = 10)

#normal
all_combined %>% 
  filter(n == 1500 & sigma == 15 & data == "normal") %>% 
  select(true, bart, rf, nn, probit, bprobit) %>% 
  ggpairs(lower = list(continuous = wrap("points", alpha = 0.2))) + 
  theme_bw() + 
  theme(text = element_text(size = 20))+
  scale_x_continuous(breaks = c(0, .5, 1))
ggsave("corr_normal.pdf")

#step
all_combined %>% 
  filter(n == 1500 & sigma == 15 & data == "step") %>% 
  select(true, bart, rf, nn, probit, bprobit) %>% 
  ggpairs(lower = list(continuous = wrap("points", alpha = 0.2))) + 
  theme_bw() + 
  theme(text = element_text(size = 20))+
  scale_x_continuous(breaks = c(0, .5, 1))
ggsave("corr_step.pdf")
#bin
all_combined %>% 
  filter(n == 1500 & sigma == 15 & data == "bin") %>% 
  select(true, bart, rf, nn, probit, bprobit) %>% 
  ggpairs(lower = list(continuous = wrap("points", alpha = 0.2))) + 
  theme_bw() + 
  theme(text = element_text(size = 20))+
  scale_x_continuous(breaks = c(0, .5, 1))
ggsave("corr_bin.pdf")
