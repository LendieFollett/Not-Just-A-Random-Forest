rm(list = ls())
library(tidyverse)
library(BART)
library(reshape2)
library(rstanarm)
library(tidyr)

source("src/functions.R")


d <- read.csv("raw/Qualtrics_FullSample_CV.csv")
str(d)

d <- d%>%
  select(CV_donate_yes, CV_donate_bid, hh_income, age,conservative_politics, female)

d <- d[complete.cases(d),]
#test set: each person / unique combination of demographics needs
#to be paired with each CV_donate_bid, and then the endpoints as well
test <- d %>%
  select(hh_income, age,conservative_politics, female)
  #filter(CV_donate_bid %in% c(25,100, 500, 1000, 2000))
test <- test[complete.cases(test),]

# Add the new columns to the `ends` data frame
#how to generalize this...
pts <- c(unique(d$CV_donate_bid),c(0, 2001))


test$ID <- 1:nrow(test)
test2 <- test[rep(1:nrow(test), each = length(pts)),] %>% 
  mutate(CV_donate_bid = rep(pts, nrow(test)))

test2 <- test2[,c("ID","CV_donate_bid", "hh_income", "age", "conservative_politics", "female")]

test_ends <- test2 %>% filter(CV_donate_bid %in% c(max(pts), min(pts)))
test_notends <- test2 %>% filter(!CV_donate_bid %in% c(max(pts), min(pts)))

p <- d %>% 
  group_by(CV_donate_bid) %>% 
  summarise(Pyes = mean(CV_donate_yes),
            n = n()) %>% 
  ggplot() + 
  geom_line(aes(x = CV_donate_bid, y = Pyes)) +
  scale_y_continuous(limits = c(0, .7))


mb <- pbart(x.train = d[,-1],
             y.train = d[,1],
             x.test = test_notends[,-1],
             ntree = 200,
             ndpost = 1000,nskip = 5000)

bprobit <- stan_glm(CV_donate_yes ~ CV_donate_bid + hh_income + age + female + conservative_politics, 
                    data = d , 
                    family = binomial(link = "probit"), 
                    prior = normal(0, 1), 
                    prior_intercept = normal(0, 1), 
                    chains = 1, iter = 2000,
                    init = "0")



library(rstanarm)
samps <- as.data.frame(bprobit)


bresults <- get_wtp(pred_matrix = (1-mb$prob.test) %>%  t() )
#lrresults <- get_wtp(pred_matrix = 1-posterior_epred(bprobit,  test_notends[,-1] ) %>% t())


bcoefs <- as.data.frame(bprobit)
WTP_bprobit <- -bcoefs["(Intercept)"] / bcoefs["A"] + as.matrix(bprobit_test[, x_list])%*%as.matrix(-bcoefs[x_list] / bcoefs["A"]) 


tdat_long2 <- bresults$wtp
tdatlr_long2 <- lrresults$wtp


#BART
 tdat_long2 %>% 
  merge(test, by = "ID") %>% 
  filter(age <80) %>% 
  ggplot() +
  geom_density(aes(x = wtp_q, fill = as.factor(hh_income)), alpha = I(.5), adjust = 2) +
  facet_grid(conservative_politics~age, scales = "free")+
  scale_fill_brewer("Income") +
  labs(title="BART") +
   theme_bw()

ggsave("bird_wtp_demographics.pdf")
p1 <- tdat_long2 %>% 
  merge(test, by = "ID") %>% 
  filter(age <80) %>% 
  ggplot() +
  geom_boxplot(aes(y = wtp_q, x = as.factor(hh_income), colour = female %>% as.factor) , alpha = I(.5)) +
  facet_wrap(~age, scales = "free")+
  theme_bw() +
  labs(x = "Income", y = "WTP (US $)")
p1

#LOGISTIC REGRESSION
p2 <- tdatlr_long2 %>%
  merge(test, by = "ID") %>% 
  ggplot() +
  geom_density(aes(x = wtp_q, fill = as.factor(hh_income)), alpha = I(.5)) +
  facet_wrap(~age, scales = "free")+
  scale_fill_brewer("Income") +
  theme_bw()+
  labs(title="Bayesian Logistic Regression")
library(gridExtra)
 grid.arrange(p1,p2, ncol = 2) 


#make demand curve

dcurves <- bresults$wtp %>% 
  group_by( variable) %>%
  mutate_at("wtp_q", list(quant = ~ 1-ecdf(.)(.))) %>%
  ungroup() 

dcurveslr <- lrresults$wtp %>% 
  group_by( variable) %>%
  mutate_at("wtp_q", list(quant = ~ 1-ecdf(.)(.))) %>%
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
write.csv(dcurves2, "bart_bird_demand_curve.csv")

dcurveslr2 <- dcurveslr%>% 
  group_by(quant = round_to_nearest_0.05(quant)) %>% 
  summarise(
    mean = median(wtp_q),
    quant05 = quantile(wtp_q, .05),
    quant95 = quantile(wtp_q, .95)) 

ggplot() +
  geom_line(aes(x = quant, y = wtp_q, group = variable), alpha = I(.1), data = dcurves) +
  #geom_line(aes(x = quant, y = wtp_q, group = variable), alpha = I(.15), data = dcurveslr, colour = "blue") +
  geom_line(aes(x = quant, y= quant05),colour = "hotpink", data = dcurves2, linetype = 2, size = 1) +
  geom_line(aes(x = quant, y= quant95),colour = "hotpink", data = dcurves2, linetype = 2, size = 1) +
  geom_line(aes(x = quant, y= mean),colour = "hotpink", data = dcurves2, size = 1) +
  geom_line(aes(x = quant, y= mean),colour = "green", data = dcurveslr2, size = 1)+
  #geom_line(aes(x = quant, y= quant05),colour = "blue", data = dcurveslr2, linetype = 2) +
  #geom_line(aes(x = quant, y= quant95),colour = "blue", data = dcurveslr2, linetype = 2) +
  labs(x = "P(WTP < A)", y = "WTP US $", caption = "BART = blue, probit = green") +
  theme_bw()
ggsave("wtp_envelope.pdf")

ggplot() +
  geom_line(aes(x = quant, y = wtp_q, group = variable), alpha = I(.1), data = dcurveslr) +
  #geom_line(aes(x = quant, y = wtp_q, group = variable), alpha = I(.15), data = dcurveslr, colour = "blue") +
  geom_line(aes(x = quant, y= quant05),colour = "red", data = dcurveslr2, linetype = 2) +
  geom_line(aes(x = quant, y= quant95),colour = "red", data = dcurveslr2, linetype = 2) +
  geom_line(aes(x = quant, y= mean),colour = "red", data = dcurveslr2) +
  labs(x = "P(WTP < A)", y = "WTP US $") +
  theme_bw()
ggsave("bird_wtp_mcmc.pdf")

p3 <- grid.arrange(p1, p2, ncol = 2, widths = c(2,1))
ggsave("birds_wtp_both.pdf", plot = p3)


dcurves2lr <- dcurveslr%>% group_by(quant = round_to_nearest_0.05(quant)) %>% 
  summarise(
    mean = mean(wtp_q),
    quant05 = quantile(wtp_q, .05),
    quant95 = quantile(wtp_q, .95))



ggplot() +
  geom_line(aes(x = quant, y= mean), data = dcurves2lr, colour = "blue") +
  geom_ribbon(aes(ymin = quant05, ymax = quant95, x= quant), alpha = .2, data = dcurves2lr, fill = "blue") +
  geom_line(aes(x = quant, y= mean), data = dcurves2, colour = "black") +
  geom_ribbon(aes(ymin = quant05, ymax = quant95, x= quant), alpha = .2, data = dcurves2, fill = "black") +
  labs(y = "WTP (US $)", x = "P(Y <= y)") + theme_bw() +
  coord_flip() +
  scale_colour_manual(name = "Group", values = c("Group 1" = "blue", "Group 2" = "black")) +
  scale_fill_manual(name = "Group", values = c("Group 1" = "blue", "Group 2" = "black")) +
  labs(y = "WTP (US $)", x = "P(Y <= y)") +
  theme_bw() +
  coord_flip()

ggsave("bird_wtp_envelope.pdf")


#re-do with numerical approximation to integral


#### PARAMETRIC MODELS 

#Linear
mpr <- stan_glm(CV_donate_yes ~ CV_donate_bid + hh_income + age, 
                data = d, 
                family = binomial(link = "probit"), 
               # prior = normal(0, 2.5), # Define priors
              #  prior_intercept = normal(0, 5), 
                chains = 1, iter = 2000)
samps <- as.data.frame(mpr)
wtp_probit <- as.matrix(samps[,c("hh_income", "age")]/ samps[,"CV_donate_bid"]) %*% t(as.matrix(test[,c(3,4)])) 

apply(wtp_probit, 2, mean) %>% hist()
apply(wtp_probit, 2, mean) %>% median

#Exponential
mpr <- stan_glm(CV_donate_yes ~ log(CV_donate_bid) + hh_income + age, 
                data = d, 
                family = binomial(link = "probit"), 
                # prior = normal(0, 2.5), # Define priors
                #  prior_intercept = normal(0, 5), 
                chains = 1, iter = 2000)

samps <- as.data.frame(mpr)
wtp_probit <- (as.matrix(samps[,c("hh_income", "age")]/samps[,"log(CV_donate_bid)"]) %*% t(as.matrix(test[,c(3,4)]))) 

x <- t((apply(samps[,c("hh_income", "age")],2, median)/median(-samps[,"log(CV_donate_bid)"])) %*% t(as.matrix(test[,c(3,4)])))

ggplot() + geom_histogram(aes(x = exp(x + .5*(1/median(samps$`log(CV_donate_bid)`)^2)))) +
  scale_x_continuous(limits = c(0,10000))

median(apply(exp(wtp_probit), 2, mean))
