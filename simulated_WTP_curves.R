rm(list = ls())
library(tidyverse)
library(BART)
library(reshape2)
library(rstanarm)
library(tidyr)

source("src/functions.R")
#introduce sparsity into prediction matrix?
sparsity = TRUE
r = reps = 1

#error variance
sigma <- 3
sigma_1 <- c(5,7,10,15)[sigma]
sigma_2 <- c(5,7,10,15)[sigma]

# Number of observations (sample size)
n <- 1500#as.numeric(substr(comb, 2, 6))

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
#for (c in c("WTP_normal", "WTP_friedman", "WTP_step", "WTP_bin")){    
c = "WTP_friedman"
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
  
  
  
  ndpost = 2000
  b <- pbart(x.train = train[, xa_list],
             y.train = train[, c],
             x.test = test_notends[, xa_list],
             ntree = 200,#cv$num_trees,
             k = 2,#cv$k,
             ndpost = ndpost, 
             nskip = 2000)
  
  tdat_long2_uncali <- get_wtp2(pred_matrix = b$prob.test %>% t(),#dim = 1000 x 1800
                                test_ends = test_ends,
                                test_notends=test_notends,
                                ndpost = ndpost)
  
  ################################################################ 
  #FIT TRADITIONAL PROBIT 
  ################################################################ 
  probit <- glm(f, data = bprobit_train, family = binomial(link = "probit"))
  coefs <- coef(probit)
  WTP_logit <- -coefs["(Intercept)"] / coefs["A"] + as.matrix(bprobit_test[, x_list])%*%as.matrix(-coefs[x_list] / coefs["A"])
  
  




library(rstanarm)
samps <- as.data.frame(bprobit)

bresults <- get_wtp(pred_matrix = (mb$prob.test) %>%  t())

bcoefs <- as.data.frame(bprobit) %>% apply(2, mean)
first <- as.vector(-samps["(Intercept)"] / samps["CV_donate_bid"])[[1]]
second <-  -as.matrix(test[, x_list])%*%t(apply(data.frame(samps[,x_list]),2, function(x){x/samps[,"CV_donate_bid"]})) 
WTP_bprobit <- apply(second, 1, function(x){x + first})

lrresults <- list(wtp = WTP_bprobit%>% as.data.frame() %>% 
                    mutate(variable = paste0("Rep", c(1:1000)))  %>% 
                    melt, 
                  mean_wtp = data.frame(wtp_q = apply(WTP_bprobit, 2, mean)))
colnames(lrresults$wtp) <- c("variable", "ID", "wtp_q")

ggplot() + geom_point(aes(test.wtp[,c], bresults$mean_wtp$wtp_q)) + geom_abline(slope = 1, intercept = 0)
ggplot() + geom_point(aes(test.wtp[,c], lrresults$mean_wtp$wtp_q)) + geom_abline(slope = 1, intercept = 0)
ggplot() + geom_point(aes(bresults$mean_wtp$wtp_q, lrresults$mean_wtp$wtp_q)) + geom_abline(slope = 1, intercept = 0)

mean((test.wtp[,c]- bresults$mean_wtp$wtp_q)^2)/mean((test.wtp[,c]- lrresults$mean_wtp$wtp_q)^2)


tdat_long2 <- bresults$wtp
tdatlr_long2 <- lrresults$wtp



#make demand curve

dcurves <- bresults$wtp %>% 
  group_by( variable) %>%
  mutate_at("wtp_q", list(quant = ~ 1-ecdf(.)(.))) %>%
  ungroup() 

dcurveslr <- lrresults$wtp %>% 
  group_by( variable) %>%
  mutate_at("wtp_q", list(quant = ~ 1-ecdf(.)(.))) %>%
  ungroup() 

dcurves_true <- test.wtp %>% 
  mutate_at(c, list(quant = ~ 1-ecdf(.)(.))) %>%
  ungroup() 

round_to_nearest_0.05 <- function(x) {
  round(x / 0.05) * 0.05
}

dcurves2 <- dcurves%>% 
  group_by(quant = round_to_nearest_0.05(quant)) %>% 
  summarise(
    mean = median(wtp_q),
    quant05 = quantile(wtp_q, .05),
    quant95 = quantile(wtp_q, .95)) 

dcurvestrue2 <- dcurves_true%>% 
  select(all_of(c("quant", c))) %>% 
  rename("WTP" = c) %>% 
  group_by(quant = round_to_nearest_0.05(quant)) %>% 
  summarise(
    mean = median(WTP),
    quant05 = quantile(WTP, .05),
    quant95 = quantile(WTP, .95)) 



dcurves2lr <- dcurveslr%>% group_by(quant = round_to_nearest_0.05(quant)) %>% 
  summarise(
    mean = mean(wtp_q),
    quant05 = quantile(wtp_q, .05),
    quant95 = quantile(wtp_q, .95))


ggplot() +
  geom_line(aes(x = quant, y= mean,colour = "Probit"), data = dcurves2lr) +
  #geom_ribbon(aes(ymin = quant05, ymax = quant95, x= quant, fill = "Probit"), alpha = .2, data = dcurves2lr) +
  geom_line(aes(x = quant, y= mean, colour = "BART"), data = dcurves2) +
  #geom_ribbon(aes(ymin = quant05, ymax = quant95, x= quant, fill = "BART"), alpha = .2, data = dcurves2) +
  geom_line(aes(x = quant, y= mean,colour = "True"), data = dcurvestrue2) +
  labs(y = "WTP (US $)", x = "P(Y <= y)") + theme_bw() +
  coord_flip() +
  scale_colour_manual(name = "Model", values = c("Probit" = "blue", "BART" = "black", "True" = "red")) +
  #scale_fill_manual(name = "Model", values = c("Probit" = "blue", "BART" = "black")) +
  labs(y = "WTP (US $)", x = "P(Y <= y)") +
  theme_bw() 

ggsave("step_wtp_envelope.pdf")


