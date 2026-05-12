
rm(list = ls())
library(tidyverse)
library(BART)
library(reshape2)
library(rstanarm)
library(tidyr)
dr <- read.csv("raw/BART_restricted.csv") %>% mutate(data = "Restricted Sample") %>% select(data, CV_donate_bid, CV_donate_yes)
df <- read.csv("raw/Qualtrics_FullSample_CV.csv") %>% mutate(data = "Full Sample")%>% select(data, CV_donate_bid, CV_donate_yes)


rbind(dr, df) %>% 
  filter(!is.na(CV_donate_bid)) %>% 
  group_by(data, CV_donate_bid) %>% 
  summarise(Pyes = mean(CV_donate_yes),
            n = n()) %>% 
  ggplot() + 
  geom_col(aes(x = as.factor(CV_donate_bid), y = Pyes, fill = data), position = "dodge") +
  scale_fill_grey("") +
  theme_bw() +
  labs(x = "Bid Amounts", y = "Proportion Accepting Bid") +
  theme(text = element_text(size = 18))
ggsave("percent_yes_responses.pdf")
