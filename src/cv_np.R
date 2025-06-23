rm(list = ls())
library(tidyverse)
library(BART)
library(reshape2)
library(rstanarm)
library(tidyr)
library(Iso)

source("src/functions.R")


d <- read.csv("/Users/000766412/Dropbox/BART/NJARF_raw/BART_restricted.csv")
#d <- read.csv("raw/Qualtrics_FullSample_CV.csv")
#d <- read.csv("raw/Qualtrics_FullSample_CV.csv")
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
  group_by(CV_donate_bid, hh_income) %>% 
  summarise(Pyes = mean(CV_donate_yes),
            n = n()) %>% 
  ggplot() + 
  geom_line(aes(x = CV_donate_bid, y = Pyes, colour = as.factor(hh_income))) +
  scale_y_continuous(limits = c(0, .7)) +
  facet_wrap(~hh_income)
p

mb <- pbart(x.train = d[,-1],
             y.train = d[,1],
             x.test = test_notends[,-1],
             ntree = 200,
             ndpost = 5000,nskip = 5000)


bprobit <- stan_glm(CV_donate_yes ~ CV_donate_bid +hh_income  + age + conservative_politics + female + age,
                    data = d , 
                    family = binomial(link = "probit"), 
                    prior = normal(0, 1), 
                    prior_intercept = normal(0, 1), 
                    chains = 1, iter = 2000,
                    init = "0")
#x_list <- c("hh_income", "age", "conservative_politics", "female")

#average WTP per person
#WTP_logit <- -coefs["(Intercept)"] / coefs["CV_donate_bid"] + as.matrix(d[, x_list])%*%as.matrix(-coefs[x_list] / coefs["CV_donate_bid"])
#d<- model.matrix(CV_donate_yes~CV_donate_bid +hh_income  + age + conservative_politics + female + age, data=d %>% select(CV_donate_bid,CV_donate_yes,hh_income  , age , conservative_politics , female , age))
#posterior sampling distributions of WTP for each person
bcoefs <- as.data.frame(bprobit)
bcoefs_x <- names(bcoefs)[-c(1,2)]
a <- t(-bcoefs["(Intercept)"] / bcoefs["CV_donate_bid"]) 
b <- as.matrix(d[,bcoefs_x]) %*%t(-bcoefs[bcoefs_x]/bcoefs[,"CV_donate_bid"])
WTP_bprobit <- (b + matrix(a, nrow = nrow(b), ncol = ncol(b), byrow = TRUE)) %>%
  data.frame() %>% 
  mutate(ID = as.character(c(1:978))) %>% 
  melt(measure.vars = paste0("X", 1:1000), value.name = "wtp_q") %>% 
  mutate(wtp_q = pmax(wtp_q, 0) )
  


WTP_bprobit_means <- WTP_bprobit %>% group_by(ID) %>% summarize(mean = mean(wtp_q)) %>% pull(mean) 


bresults <- get_wtp(pred_matrix = mb$prob.test %>%  t() )

lrresults <- get_wtp(pred_matrix = posterior_epred(bprobit,  test_notends[,-1] ) %>% t())
#lrresults <- list(wtp =WTP_bprobit,
#                  mean_wtp = WTP_bprobit_means)

tdat_long2 <- bresults$wtp
tdatlr_long2 <- lrresults$wtp
#ran this analysis with get_wtp and get_wtp2 - those provide same estimates
#both are very different from traditional formula

#problem: using the traditional probit WTP formula (coefficient based)
#we get very different WTP estimates than taking that 
#same probit model and using quadtrature methods
#next step: differences between get_wtp and get_wtp2?


tdat_long2 %>% 
  merge(test, by = "ID") %>% 
  filter(age <80) %>% 
  mutate(age_f = factor(age, levels = c(20,30,40,50,60,70),
                        labels = c("18-24", "25-34", "35-44",
                                   "45-54", "55-64", "65-74"))) %>% 
  ggplot() +
  geom_boxplot(aes(y = wtp_q, x = as.factor(hh_income), colour = conservative_politics %>% as.factor) , alpha = I(.5)) +
  facet_wrap(~age_f, scales = "free")+
  theme_bw(base_size = 16)+
  scale_colour_grey("Conserv.\nPolitics")+
  labs(x = "Income", y = "WTP (US $)") +
  ggtitle("BART-based WTP estimates")
ggsave("bart_exploratory.pdf", width = 12, height = 10)

tdatlr_long2 %>% 
  merge(test, by = "ID") %>% 
  filter(age <80) %>% 
  ggplot() +
  geom_boxplot(aes(y = wtp_q, x = as.factor(hh_income), colour = conservative_politics %>% as.factor) , alpha = I(.5)) +
  facet_wrap(~age, scales = "free")+
  scale_colour_brewer("Politics", type = "qual")+
  theme_bw() +
  labs(x = "Income", y = "WTP (US $)")+
  ggtitle("Probit-based WTP estimates")
ggsave("probit_exploratory.pdf")

#table
table_dat <- tdat_long2 %>% 
  merge(test, by = "ID") %>% 
  group_by(hh_income, age, conservative_politics, female) %>% 
  summarise(mean_wtp = mean(wtp_q),
            lower = quantile(wtp_q, .025),
            upper = quantile(wtp_q, .975))

write.csv(table_dat, "wtp_posteriors_table.csv", row.names=FALSE)


group_vars <- c("hh_income", "age", "conservative_politics", "female")

summary_list <- lapply(group_vars, function(var) {
  tdat_long2 %>%
    merge(test, by = "ID") %>%
    group_by(.data[[var]]) %>%
    summarise(mean_wtp = mean(wtp_q),
              lower = quantile(wtp_q, .025),
              upper = quantile(wtp_q, .975),
              .groups = "drop") %>%
    mutate(grouping_variable = var,
           model = "BART") %>%
    rename(group_value = !!sym(var))
})


summary_listlr <- lapply(group_vars, function(var) {
  tdatlr_long2 %>%
    merge(test, by = "ID") %>%
    group_by(.data[[var]]) %>%
    summarise(mean_wtp = mean(wtp_q),
              lower = quantile(wtp_q, .025),
              upper = quantile(wtp_q, .975),
              .groups = "drop") %>%
    mutate(grouping_variable = var,
           model = "Bayesian Probit") %>%
    rename(group_value = !!sym(var))
})
names(summary_list) <- group_vars
names(summary_listlr) <- group_vars

vars = c("hh_income", "age", "conservative_politics", "female")
x <- vars[1]
rbind(summary_list[[x]],summary_listlr[[x]]) %>% 
ggplot( aes(x = factor(group_value), y = mean_wtp, fill = model)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "Income", y = "WTP", fill = "Model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_grey() +
  theme(text=element_text(size = 20)) 
ggsave("income_plot.pdf")


bart_summaries <- tdat_long2 %>% 
  merge(test, by = "ID") %>% 
  filter(age <80) %>% 
  mutate(age_f = factor(age, levels = c(20,30,40,50,60,70),
                        labels = c("18-24", "25-34", "35-44",
                                   "45-54", "55-64", "65-74"))) %>% 
  group_by(age_f, hh_income) %>% 
  summarise(mean_wtp = mean(wtp_q),
            lower = quantile(wtp_q, .025),
            upper = quantile(wtp_q, .975),
            .groups = "drop") 
lr_summaries <- tdatlr_long2 %>% 
  merge(test, by = "ID") %>% 
  filter(age <80) %>% 
  mutate(age_f = factor(age, levels = c(20,30,40,50,60,70),
                        labels = c("18-24", "25-34", "35-44",
                                   "45-54", "55-64", "65-74"))) %>% 
  group_by(age_f, hh_income) %>% 
  summarise(mean_wtp = mean(wtp_q),
            lower = quantile(wtp_q, .025),
            upper = quantile(wtp_q, .975),
            .groups = "drop") 

rbind(data.frame(bart_summaries, model = "BART"),
      data.frame(lr_summaries, model = "Bayesian Probit"))%>% 
  ggplot( aes(x = as.factor(hh_income), y = mean_wtp, fill = model)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "Income", y = "WTP", fill = "Model") +
  facet_wrap(~age_f) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_grey() +
  theme(text=element_text(size = 20))  
ggsave("both_exploratory.pdf", width = 20, height = 15)
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
  geom_line(aes(x = quant, y= quant05),colour = "red", data = dcurves2, linetype = 2, size = 1) +
  geom_line(aes(x = quant, y= quant95),colour = "red", data = dcurves2, linetype = 2, size = 1) +
  geom_line(aes(x = quant, y= mean),colour = "red", data = dcurves2, size = 1) +
  #geom_line(aes(x = quant, y= mean),colour = "green", data = dcurveslr2, size = 1)+
  #geom_line(aes(x = quant, y= quant05),colour = "blue", data = dcurveslr2, linetype = 2) +
  #geom_line(aes(x = quant, y= quant95),colour = "blue", data = dcurveslr2, linetype = 2) +
  labs(x = "P(WTP < A)", y = "WTP US $") +
  theme_bw(base_size = 18)
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
  #coord_flip() +
  scale_colour_manual(name = "Group", values = c("Group 1" = "blue", "Group 2" = "black")) +
  scale_fill_manual(name = "Group", values = c("Group 1" = "blue", "Group 2" = "black")) +
  labs(y = "WTP (US $)", x = "P(Y <= y)") +
  theme_bw() +
  #coord_flip() +
  labs(caption = "BART (black)\nProbit (blue)")

ggsave("bird_wtp_envelope.pdf")



#### PARAMETRIC MODELS 

#Linear
mpr <- stan_glm(CV_donate_yes ~ CV_donate_bid + hh_income + age, 
                data = d, 
                family = binomial(link = "probit"), 
               # prior = normal(0, 2.5), # Define priors
              #  prior_intercept = normal(0, 5), 
                chains = 1, iter = 2000)
samps <- as.data.frame(mpr)
wtp_probit <- as.matrix(-samps[,c("(Intercept)")]/ samps[,"CV_donate_bid"]) + as.matrix(samps[,c("hh_income", "age")]/ samps[,"CV_donate_bid"]) %*% t(as.matrix(d[,c(3,4)])) 

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
