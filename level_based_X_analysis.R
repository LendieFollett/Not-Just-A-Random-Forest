rm(list = ls())
library(tidyverse)
library(BART)
d <- read.csv("int03_BART_5Jul2023.csv")
str(d)

d <- d %>% 
  mutate(otter_low = 1- (otter_25 + otter_50 + otter_75),
         otter_med = otter_25 + otter_50,
         otter_high = otter_75,
         
         whale_low = 1-(whale_50 + whale_75 + whale_100),
         whale_med = whale_50 + whale_75,
         whale_high = whale_100,
         
         orca_low = 1-(orca_5 + orca_10 + orca_15),
         orca_med = orca_5 + orca_10,
         orca_high = orca_15,
         
         otter_percent = case_when(otter_25 == 1 ~ 25,
                                   otter_50 == 1 ~ 50,
                                   otter_75 == 1 ~ 75,
                                   TRUE ~ 0),
         whale_percent = case_when(whale_50 == 1 ~ 50,
                                   whale_75 == 1 ~ 75,
                                   whale_100 == 1 ~ 100,
                                   TRUE ~ 0),
         orca_percent = case_when(orca_5 == 1 ~ 5,
                                   orca_10 == 1 ~ 10,
                                   orca_15 == 1 ~ 15,
                                   TRUE ~ 0)
         ) %>%
  select(-c(otter_25, otter_50 ,otter_75,whale_50, whale_75, whale_100,orca_5 ,orca_10, orca_15))
keeps <- c("cost", "otter_low", "otter_med" ,"otter_high","whale_low", "whale_med", "whale_high","orca_low" ,"orca_med", "orca_high")
distinct <- d[!duplicated(d[,keeps]),] 

distinct <- distinct %>%
  mutate(product = factor(as.integer(factor(paste0(cost, otter_low, otter_med, otter_high, whale_low, whale_med, whale_high, orca_low, orca_med, orca_high)))),
         ref = paste(cost, otter_low, otter_med, otter_high, whale_low, whale_med, whale_high, orca_low, orca_med, orca_high, sep = "_"))
head(distinct)

d <- d %>%
  mutate(product = factor(as.integer(factor(paste0(cost, otter_low, otter_med, otter_high, whale_low, whale_med, whale_high, orca_low, orca_med, orca_high))))) 

all <- d %>% 
  filter(!(otter_low == 1 & whale_low == 1 & orca_low == 1)) %>%
  group_by(grp) %>%
  summarise(selectedchoice = as.factor(sum(selectedchoice*as.integer(as.character(product)))),
            max_otter_percent = max(otter_percent),
            max2_otter_percent = min(otter_percent),
            
            max_orca_percent = max(orca_percent),
            max2_orca_percent = min(orca_percent),
            
            max_whale_percent = max(whale_percent),
            max2_whale_percent = min(whale_percent),
            
            max_cost = max(cost),
            max2_cost = min(cost))%>% as.data.frame()

test <- distinct %>% 
  filter(!(otter_low == 1 & whale_low == 1 & orca_low == 1)) %>%
  group_by(grp) %>%
  summarise(max_otter_percent = max(otter_percent),
            max2_otter_percent = min(otter_percent),
            
            max_orca_percent = max(orca_percent),
            max2_orca_percent = min(orca_percent),
            
            max_whale_percent = max(whale_percent),
            max2_whale_percent = min(whale_percent),
            
            max_cost = max(cost),
            max2_cost = min(cost)) %>% as.data.frame()


mb <- mbart2(x.train = all[,-(1:2)],
             y.train = all$selectedchoice,
             x.test = test[,-1],
             type = 'pbart',
             ntree = 100,
             ndpost = 1000,nskip = 100)

