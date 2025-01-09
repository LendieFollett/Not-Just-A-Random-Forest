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
library(bartMachine)
library(logistf)
source("functions.R")

numCores <- detectCores() - 1  # Use one less than the total number of cores
cl <- makeCluster(numCores)
registerDoParallel(cl)

sigma <- c(1,2,3,4) 
n <- c(1000, 500) + 500
#reserve 500 for the test set

comb <- expand.grid(sigma,n) %>% mutate(keep = paste0(Var1, Var2)) %>% pull(keep)

registerDoParallel(cores = 4)  # Adjust the number of cores as needed

results <- foreach(comb = comb, .packages = c('BART', 'rstanarm')) %:%
  foreach(reps = 25) %dopar% {
    #introduce sparsity into prediction matrix?
    sparsity = TRUE
    
    #error variance
    sigma <- as.numeric(substr(comb, 1, 1))
    sigma_1 <- c(5,7,10,15)[sigma]#c(5,10,15)[sigma] #small, medium, large
    sigma_2 <- c(5,7,10,15)[sigma]
    
    # Number of observations (sample size)
    n <- as.numeric(substr(comb, 2, 6))
    
    r <- reps
    # Load necessary data and scripts
    # Set seed for reproducibility
    set.seed(r*sigma_1*n + n + sigma_1 + r)
    
    #number of x variables
    nP <- 10
  
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
    
    # Binary x's only, normal error
    bin_beta <- c(5,10,-15,-25,10,25, 15) %>% as.matrix()
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
    A <- c(25, 50, 75, 100, 125, 150)
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
      } else{ #sparsity ==FALSE
        if (! c   %in% c("WTP_friedman", "WTP_bin")) { # Case when the column is NOT "WTP_friedman"
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
        bprobit_test <- test %>%
          mutate(across(starts_with("X"), ~ (. - train_means[[cur_column()]]) / train_sds[[cur_column()]]))
        
        bprobit_test_ends <- test_ends %>%
          mutate(across(starts_with("X"), ~ (. - train_means[[cur_column()]]) / train_sds[[cur_column()]]))
        
        bprobit_test_notends <- test_notends %>%
          mutate(across(starts_with("X"), ~ (. - train_means[[cur_column()]]) / train_sds[[cur_column()]]))
        
      }else{
        bprobit_test <- test
        bprobit_train <- train
        bprobit_test_ends <- test_ends
        bprobit_test_notends <- test_notends
        
      }   
      
      
      n_train <- bprobit_train
      n_test <- bprobit_test
      n_test_ends <- bprobit_test_ends
      n_test_notends <- bprobit_test_notends
      
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
                 ntree = 200,#cv$num_trees,
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
      
      if(r == 1){
      all[[j]] <- data.frame(n = n*.7, 
                              data = gsub("WTP_", "", c), 
                              rep = r,
                              sigma = sigma_1,
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
      results[[j]] <- data.frame(n = n*.7,
                                 data = gsub("WTP_", "", c),
                                 sigma = sigma_1,
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
                                 bart_bias = median(tdat_long2$wtp_q) - median(test.wtp[, c]),
                                 bart_uncali_bias = median(tdat_long2_uncali$wtp_q) - median(test.wtp[, c]),
                                 nn2_bias = median(tdatn2_long2$wtp_q) - median(test.wtp[, c]),
                                 rf_bias = median(tdatrf_long2$wtp_q) - median(test.wtp[, c]),
                                 rf_uncali_bias = median(tdatrf_uncali_long2$wtp_q) - median(test.wtp[, c]),
                                 probit_bias = median(WTP_logit) - median(test.wtp[, c]),
                                 bprobit_bias = median(WTP_bprobit) - median(test.wtp[, c]))
      
    }
    all <- do.call(rbind, all)
    results <- do.call(rbind, results)
    
    list( results = results)#all = all,
  }



#all_combined <- do.call(rbind, lapply(results, function(x) do.call(rbind, lapply(x, function(y) y$all))))
results_combined <- do.call(rbind, lapply(results, function(x) do.call(rbind, lapply(x, function(y) y$results))))


sparsity = FALSE
saveRDS(all_combined, paste0("all_combined_",sparsity,".RDS"))
saveRDS(results_combined, paste0("results_combined_",sparsity,".RDS"))

results_combinedTRUE <- readRDS( paste0("results_combined_",TRUE,".RDS")) %>% mutate(kind = "Sparse")
results_combinedFALSE <- readRDS( paste0("results_combined_",FALSE,".RDS")) %>% mutate(kind = "Not Sparse")

results_combined <-results_combinedFALSE
# Calculate results with highlighting of the lowest point
highlighted_points <- results_combined %>%
  group_by(data, sigma, n) %>%
  summarise(
   # bart_cali = mean(bart_mse) / mean(probit_mse),
    bart_uncali = mean(bart_uncali_mse) / mean(probit_mse),
    nn2_q = mean(nn2_mse) / mean(probit_mse),
    rf_cali = mean(rf_mse) / mean(probit_mse),
    rf_uncali = mean(rf_uncali_mse) / mean(probit_mse),
    bprobit = mean(bprobit_mse) / mean(probit_mse)
  ) %>%
  mutate(data = factor(data, levels = c("normal", "friedman", "step", "bin"),
                       labels = c("Linear", "Friedman", "Step", "Binary"))) %>%
  pivot_longer(cols = c(bart_uncali, nn2_q,rf_cali,rf_uncali, bprobit), names_to = "model", values_to = "value") %>%
  group_by(data, sigma,n) %>%
  filter(value == min(value)) %>% # Filter for the minimum value for each sigma and data combination
  ungroup()

# COMPARING RMSE - need to also look at bias
results_combined %>%
  group_by(data, sigma,n) %>%
  summarise(
    #bart_cali = mean(bart_mse) / mean(probit_mse),
    bart_uncali = mean(bart_uncali_mse) / mean(probit_mse),
    nn2_q = mean(nn2_mse) / mean(probit_mse),
    rf_cali = mean(rf_mse) / mean(probit_mse),
    rf_uncali = mean(rf_uncali_mse) / mean(probit_mse),
    bprobit = mean(bprobit_mse) / mean(probit_mse)
  ) %>% 
  ungroup() %>%
  mutate(data = factor(data, levels = c("normal", "friedman", "step", "bin"),
                       labels = c("Linear", "Friedman", "Step", "Binary"))) %>%
  ggplot() +
  #geom_line(aes(x = sigma, y = bart_cali, colour = "BART Cali")) +
  #geom_point(aes(x = sigma, y = bart_cali, colour = "BART Cali")) +
  
  geom_line(aes(x = sigma, y = nn2_q, colour = "NN")) +
  geom_point(aes(x = sigma, y = nn2_q, colour = "NN")) +
  
  geom_line(aes(x = sigma, y = bprobit, colour = "bprobit")) +
  geom_point(aes(x = sigma, y = bprobit, colour = "bprobit")) +
  
  geom_line(aes(x = sigma, y = rf_cali, colour = "RF Cali")) +
  geom_point(aes(x = sigma, y = rf_cali, colour = "RF Cali")) +
  
  geom_line(aes(x = sigma, y = rf_uncali, colour = "RF Uncali")) +
  geom_point(aes(x = sigma, y = rf_uncali, colour = "RF Uncali")) +
  
  geom_line(aes(x = sigma, y = bart_uncali, colour = "BART Uncali")) +  
  geom_point(aes(x = sigma, y = bart_uncali, colour = "BART Uncali")) +

  geom_hline(aes(yintercept = 1, colour = "Probit"), linetype = 2) +
  
  geom_point(data = highlighted_points, 
             aes(x = sigma, y = value, colour = model), 
             size = 4, shape = 1) + # Highlighted points
  facet_grid(n~data) +
  scale_color_manual(
    name = "Model", # Legend title
    #values = c("BART" = "grey10", "NN" = "grey50", "RF" = "grey80", "Probit" = "grey"),
    #breaks = c("BART", "NN", "RF", "Probit")
    values = c("BART Cali" = "blue", "BART Uncali" = "lightblue","RF Cali" = "darkgreen", "RF Uncali" = "lightgreen",
               "NN" = "purple",bprobit = "pink",Probit = "grey")
  ) +
  theme_bw() +
  labs(x = expression(sigma[epsilon]), y = "RMSE Ratio\n(relative to Probit)") +
  theme(text=element_text(size = 20))

ggsave("RMSE_ratios_notsparse.pdf", width = 20, height = 15)



results_combined %>% 
  group_by(data) %>% 
  summarise(diff = t.test(bart_q, probit, paired = TRUE)$estimate,
            statistic = t.test(bart_q, probit, paired = TRUE)$statistic,
            p.value = t.test(bart_q, probit, paired = TRUE)$p.value
  )%>% 
  xtable(digits = c(1,10,2,2,-1))



results_combined[,-8]%>%
  melt(id.vars = c("data", "rep", "sigma")) %>% 
  #filter(!grepl("bp", variable)) %>% 
  ggplot() +
  geom_boxplot(aes(x = factor(sigma), y = value, colour = variable)) +
  facet_wrap(data~., scales = "free_y") +
  labs(x = "Sigma", y = "RMSE")


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


##########

all_combinedTRUE <- readRDS( paste0("all_combined_",TRUE,".RDS")) %>% mutate(kind = "Sparse")
all_combinedFALSE <- readRDS( paste0("all_combined_",FALSE,".RDS")) %>% mutate(kind = "Not Sparse")

all_combined <- all_combinedTRUE

#in terms of median predicted WTP
q <- .5
all_summarised <- all_combined %>% 
  arrange(data, sigma,n) %>% 
  group_by(data, sigma,n) %>% 
  mutate(rep = rep(1:25, each = first(n)*.3)) %>% 
  group_by(data, sigma, rep,n) %>% 
  #median modeled and true WTP for test sets, for each rep
  summarise(bart_q = quantile(bart_q,q) %>% as.numeric(),
            nn2_q = quantile(nn2_q,q)%>% as.numeric(),
            rf = quantile(rf,q)%>% as.numeric(),
            probit = quantile(probit,q)%>% as.numeric(),
            true = quantile(data.1,q)%>% as.numeric()) %>% 
  ungroup() 


all_summarised%>% 
  melt(id.vars = c("data", "sigma", "rep", "true", 'n')) %>% 
  filter(data == "WTP_normal") %>% 
  ggplot() +
  geom_point(aes(x = true, y = value, colour = factor(sigma))) +
  geom_smooth(aes(x = true, y = value, colour = factor(sigma)), method = "lm", se = FALSE) +
  facet_grid(n~variable) +
  geom_abline(aes(slope = 1, intercept = 0)) +
  coord_equal()+
  scale_colour_brewer(palette = "Blues")+
  labs(x = "True Median WTP", y = "Predicted Median WTP") +
  theme_dark()
  
#Mean absolute deviation in terms of test median
all_summarised %>% 
  group_by(data, sigma, n) %>% 
  summarise(
    bart_q = mean(abs(bart_q - true)) / mean(abs(probit - true)),
    nn2_q = mean(abs(nn2_q - true)) / mean(abs(probit - true))#,
   # rf = mean(abs(rf - true)) / mean(abs(probit - true))
  ) %>% 
  melt(id.vars = c("data", "sigma", 'n'))%>% 
  ggplot()+
  geom_point(aes(x = sigma, y = value, colour =variable )) + 
  geom_line(aes(x = sigma, y = value, colour =variable )) + 
  geom_hline(aes(yintercept = 1))+
  facet_grid(n~data) 

all_summarised %>% 


#### ASSESSING BIAS

bias_summarised <- all_combined %>% 
  arrange(data, sigma,n) %>% 
  group_by(data, sigma,n) %>% 
  mutate(rep = rep(1:25, each = first(n)*.3)) %>% 
  group_by(data, sigma,n, rep) %>% 
  summarise(bart_q = mean(bart_q - data.1),
            nn2_q = mean(nn2_q - data.1),
            rf = mean(rf - data.1),
            probit = mean(probit - data.1)
) %>% 
  ungroup()%>% 
  melt(id.vars = c("data", "sigma", "rep", 'n'))

bias_summarised %>% 
  filter(n == 500) %>% 
  ggplot() + 
  geom_violin(aes(y = value, x = variable, colour = variable), alpha = I(.5)) + 
  geom_hline(aes(yintercept = 0), linetype = 2, colour = "grey50") +
  facet_grid(data~sigma) +
  theme_bw() + 
  scale_fill_brewer(palette = "Spectral")

