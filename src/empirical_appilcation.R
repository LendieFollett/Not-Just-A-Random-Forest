rm(list = ls())
library(tidyverse)
library(BART)
library(reshape2)
library(rstanarm)
library(tidyr)
library(Iso)
library(caret)

source("src/functions.R")


d <- read.csv("/Users/lendie.follettdrake.edu/Library/CloudStorage/Dropbox/BART/NJARF_raw/BART_restricted.csv")

str(d)



d <- d%>%
  select(CV_donate_yes, CV_donate_bid, hh_income, age,conservative_politics, female) 

d <- d[complete.cases(d),]
#test set: each person / unique combination of demographics needs
#to be paired with each CV_donate_bid, and then the endpoints as well
test <- d %>%
  select(hh_income, age,conservative_politics, female)
test <- test[complete.cases(test),]

# Add the new columns to the `ends` data frame
pts <- c(unique(d$CV_donate_bid),c(0, 2001))


test$ID <- 1:nrow(test)
#repeat each unique row with each bid amount
test2 <- test[rep(1:nrow(test), each = length(pts)),] %>% 
  mutate(CV_donate_bid = rep(pts, nrow(test)))

test2 <- test2[,c("ID","CV_donate_bid", "hh_income", "age", "conservative_politics", "female")]

test_ends <- test2 %>% filter(CV_donate_bid %in% c(max(pts), min(pts)))
test_notends <- test2 %>% filter(!CV_donate_bid %in% c(max(pts), min(pts)))

p <- d %>% 
  group_by(CV_donate_bid) %>% 
  summarise(Pyes = mean(CV_donate_yes),
            n = n()) %>% 
  rbind(data.frame(CV_donate_bid = 0, Pyes = 1, n = 0))

interpol <- lm(Pyes ~ CV_donate_bid, data = p)
upper <- -coef(interpol)[1]/coef(interpol)[2]
#3404.015

p%>% 
  ggplot() + 
  geom_line(aes(x = CV_donate_bid, y = Pyes)) +
  scale_y_continuous(limits = c(0, .7)) 

######################################
# BART--------------------
######################################
mb <- pbart(x.train = d[,-1],
            y.train = d[,1],
            x.test = test_notends[,-1],
            ntree = 200,
            ndpost = 4000,nskip = 5000)

######################################
# BAYESIAN PROBIT--------------------
######################################
bprobit <- stan_glm(CV_donate_yes ~ CV_donate_bid +hh_income  + age + conservative_politics + female ,
                    data = d , 
                    family = binomial(link = "probit"), 
                    prior = normal(0, 1), 
                    prior_intercept = normal(0, 1), 
                    chains = 4, iter = 2000,
                    init = "0")




#posterior sampling distributions of WTP for each person
bcoefs <- as.data.frame(bprobit)
bcoefs_x <- names(bcoefs)[-c(1,2)]
a <- t(-bcoefs["(Intercept)"] / bcoefs["CV_donate_bid"]) 
b <- as.matrix(d[,bcoefs_x]) %*%t(-bcoefs[bcoefs_x]/bcoefs[,"CV_donate_bid"])
WTP_bprobit <- (b + matrix(a, nrow = nrow(b), ncol = ncol(b), byrow = TRUE)) %>%
  data.frame() %>% 
  mutate(ID = as.character(c(1:978))) %>% 
  melt(measure.vars = paste0("X", 1:4000), value.name = "wtp_q") #%>% 
# mutate(wtp_q = pmax(wtp_q, 0) )



WTP_bprobit_means <- WTP_bprobit %>% group_by(ID) %>% summarize(mean = mean(wtp_q)) %>% pull(mean) 

#transform probabilities into WTP for each MCMC sample
bresults <- get_wtp(pred_matrix = mb$prob.test %>%  t() )
lrresults <- get_wtp(pred_matrix = posterior_epred(bprobit,  test_notends[,-1] ) %>% t())


tdat_long2 <- bresults$wtp
tdatlr_long2 <- lrresults$wtp

temp <- rbind(data.frame(tdat_long2, model = "BART"),
              data.frame(tdatlr_long2, model = "Probit")) %>% 
  merge(test, by = "ID") %>% 
  group_by(model, hh_income, age,conservative_politics, female) %>% 
  summarise(mean = mean(wtp_q),
            lower = quantile(wtp_q, .025),
            upper = quantile(wtp_q, .975)) 

temp%>% 
  filter(hh_income < 175 & hh_income != 30 & model != "NN") %>% #taking out levels to clean up graph
  mutate(female = factor(female, levels = c(0,1), labels = c("Male", "Female")),
         conservative_politics = factor(conservative_politics, levels = c(0,1), labels = c("Not Conservative", "Conservative"))) %>% 
  ggplot() +
  geom_line(aes(x = age, y = mean, linetype = model, colour = hh_income, group = interaction(hh_income, model))) +
  facet_grid(female~conservative_politics) +
  theme_bw() +
  scale_colour_continuous("Household\nIncome", palette = c("grey20", "grey90"))+
  scale_linetype("Model") +
  labs(x = "Age", y = "Estimated WTP")
ggsave("bart_probit_compare.pdf", width = 12, height = 10)


#table
table_dat <- tdat_long2 %>% 
  merge(test, by = "ID") %>% 
  group_by(hh_income, age, female) %>% 
  summarise(mean_wtp = mean(wtp_q),
            lower = quantile(wtp_q, .025),
            upper = quantile(wtp_q, .975))

write.csv(table_dat, "wtp_posteriors_table.csv", row.names=FALSE)


table_datlr <- tdatlr_long2 %>% 
  merge(test, by = "ID") %>% 
  group_by(hh_income, age, female) %>% 
  summarise(mean_wtp = mean(wtp_q),
            lower = quantile(wtp_q, .025),
            upper = quantile(wtp_q, .975))

write.csv(table_datlr, "wtp_posteriors_table_BPROBIT.csv", row.names=FALSE)


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
  group_by(age_f, hh_income,female ) %>% 
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
  group_by(age_f, hh_income, female) %>% 
  summarise(mean_wtp = mean(wtp_q),
            lower = quantile(wtp_q, .025),
            upper = quantile(wtp_q, .975),
            .groups = "drop") 

rbind(#data.frame(bart_summaries, model = "BART")
  data.frame(lr_summaries, model = "Bayesian Probit")
)%>% 
  mutate(female = factor(female, levels = c(1,0), labels = c("Female", "Male"))) %>% 
  ggplot( aes(x = as.factor(hh_income), y = mean_wtp, fill = female)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "Income", y = "WTP", fill = "Gender") +
  facet_wrap(~age_f) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_grey() +
  theme(text=element_text(size = 20))  
ggsave("bart_exploratory.pdf", width = 20, height = 15)

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



dcurves2lr <- dcurveslr%>% 
  group_by(quant = round_to_nearest_0.05(quant)) %>% 
  summarise(
    mean = median(wtp_q),
    quant05 = quantile(wtp_q, .05),
    quant95 = quantile(wtp_q, .95))



# Add method variable to the data frames
dcurves2lr$method <- "Bayesian\n Probit"
dcurves2$method <- "BART"

# Combine into one data frame
dcurves_combined <- rbind(dcurves2lr, dcurves2)

# Plot
p1 <- ggplot(dcurves_combined, aes(x = quant, y = mean, color = method)) +
  geom_line() +
  geom_ribbon(aes(ymin = quant05, ymax = quant95, fill = method), alpha = 0.3, color = NA) +
  scale_color_manual(name = "Model", values = c("Bayesian\n Probit" = "grey70", "BART" = "grey10")) +
  scale_fill_manual(name = "Model", values = c("Bayesian\n Probit" = "grey70", "BART" = "grey10")) +
  labs(y = "WTP (US $)", x = "P(Y ≤ y)") +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none")
p1
ggsave("bird_wtp_envelope.pdf")


p2 <- ggplot() +
  geom_line(aes(x = quant, y = wtp_q, group = variable),alpha = .3, data = filter(dcurves, variable %in% paste0("Rep",1:100))) +
  geom_line(aes(x = quant, y = wtp_q, group = variable),alpha = .3,colour = "grey", data = filter(dcurveslr, variable %in% paste0("Rep",1:100)))  +
  theme_bw() +
  labs(y = "WTP (US $)", x = "P(Y ≤ y)") +
  theme(text = element_text(size = 20),
        legend.position = "none")
p2
ggsave("bird_wtp_envelope_step1.pdf")


p3 <- grid.arrange(p2, p1,ncol=2,
                   widths = c(3,3))
ggsave(filename = "bird_wtp_both.pdf", plot = p3)

