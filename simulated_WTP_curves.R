rm(list = ls())
library(tidyverse)
library(BART)
library(reshape2)
library(rstanarm)
library(tidyr)

source("functions.R")
sparsity = TRUE
c <- "WTP_friedman"
r = 2580
# Load necessary data and scripts
# Set seed for reproducibility
set.seed(1234 + r)

# Number of observations
n <- 1000

#number of x variables
nP <- 10

# standard deviation
sigma_1 <- 5
sigma_2 <- 1
#intercept 
beta_N0 <- 90 
beta_linear <- 35

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
A <- c(25, 50, 75, 100, 125, 135)
A_samps <- sample(A, size = nrow(data), replace=TRUE) %>% as.matrix

survey <- data.frame(X = X,
                     CV_donate_bid = A_samps,
                     apply(data, 2, function(x){ifelse(x > A_samps, 1, 0)}))

train.idx <- sample(1:nrow(survey), size = .7*nrow(survey), replace=FALSE)

train <- survey[train.idx,]
test <-survey[-train.idx,]
test.wtp <- data[-train.idx,]

pts <- c(unique(survey$CV_donate_bid),c(0, max(survey$CV_donate_bid) + 1))


test$ID <- 1:nrow(test)
test2 <- test[rep(1:nrow(test), each = length(pts)),] %>% 
  mutate(CV_donate_bid = rep(pts, nrow(test)))

test2 <- test2[,c("ID","CV_donate_bid", paste0("X.",1:10))]

test_ends <- test2 %>% filter(CV_donate_bid %in% c(max(pts), min(pts)))
test_notends <- test2 %>% filter(!CV_donate_bid %in% c(max(pts), min(pts)))

#sparsity
formula <- paste0(c," ~CV_donate_bid + X.1 + X.2 + X.3 + X.4 + X.5 +X.6 + X.7 + X.8 + X.9 + X.10")
xa_list <- c("CV_donate_bid", paste0("X.", 1:10))
x_list <- c(paste0("X.", 1:10))

if(sparsity == FALSE){
if(c == "WTP_friedman"){
  formula <- paste0(c," ~CV_donate_bid + X.1 + X.2 + X.3 + X.4 + X.5 ")
  xa_list <- c("CV_donate_bid", paste0("X.", 1:5))
  x_list <- c(paste0("X.", 1:5))
} else if(c == "WTP_step"){
  formula <- paste0(c," ~CV_donate_bid + X.1 ")
  xa_list <- c("CV_donate_bid", paste0("X.", 1))
  x_list <- c(paste0("X.", 1:1)) 
}
}

mb <- pbart(x.train = train[,xa_list],
            y.train = train[,c],
            x.test = test_notends[,xa_list],
            ntree = 100,
            ndpost = 1000,nskip = 5000)


bprobit <- stan_glm( formula,
                    data = train , 
                    family = binomial(link = "probit"), 
                    prior = normal(0, 1), 
                    prior_intercept = normal(0, 1), 
                    chains = 1, iter = 2000,
                    init = "0")



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


