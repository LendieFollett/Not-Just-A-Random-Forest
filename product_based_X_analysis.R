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
         orca_high = orca_15) %>%
  select(-c(otter_25, otter_50 ,otter_75,whale_50, whale_75, whale_100,orca_5 ,orca_10, orca_15))
keeps <- c("cost", "otter_low", "otter_med" ,"otter_high","whale_low", "whale_med", "whale_high","orca_low" ,"orca_med", "orca_high")
distinct <- d[!duplicated(d[,keeps]),c("grp", keeps)] 
distinct <- distinct %>%
  mutate(product = factor(as.integer(factor(paste0(cost, otter_low, otter_med, otter_high, whale_low, whale_med, whale_high, orca_low, orca_med, orca_high)))),
         ref = paste(cost, otter_low, otter_med, otter_high, whale_low, whale_med, whale_high, orca_low, orca_med, orca_high, sep = "_"))
head(distinct)
d <- d %>%
  mutate(product = factor(as.integer(factor(paste0(cost, otter_low, otter_med, otter_high, whale_low, whale_med, whale_high, orca_low, orca_med, orca_high)))),
         ref = paste(cost, otter_low, otter_med, otter_high, whale_low, whale_med, whale_high, orca_low, orca_med, orca_high, sep = "_"))

d_prod <- model.matrix(selectedchoice ~ -1 + ., data = d[,c("selectedchoice", "product")]) %>%
  as.data.frame() %>%
  mutate(grp = d$grp) %>%
  group_by(grp)%>% 
  summarise_each(list(sum))

d_price <- d_prod
y <- rep(NA, nrow(d_prod))
for (r in 1:nrow(d_price)){
  print(r)
  idx = 0
  dis_grp <- d_prod[r, "grp"]
  for (c in colnames(d_price)[-1]){
  idx = idx + 1  
    if (d_prod[r,c] == 1){
    d_price[r,c] <- distinct %>% filter(product == as.character(idx)) %>% select(cost)
    y[r] <- d %>% filter(grp == as.numeric(dis_grp) & selectedchoice == 1) %>% select(product) %>% as.numeric
    }else{
      d_price[r,c] <- 1000000000 # ~ infinity
    }
  }
}

colnames(d_price) <- c("grp", paste0("price", 1:55))
all <- cbind(y,d_prod[,-1],d_price[,-1])
all$y <- as.factor(all$y)



#create 'testing' set
d_prod_test <- model.matrix(selectedchoice ~ -1 + ., data = d[,c("selectedchoice", "product")]) %>%
  as.data.frame() %>%
  mutate(grp = d$grp) %>%
  group_by(grp)%>% 
  mutate_each(list(sum))



test = distinct(all[,-1])

mb <- mbart2(x.train = all[,-c(1,2)],
             y.train = all[,1],
             x.test = test,
             type = 'pbart',
             ntree = 100,
             ndpost = 1000,nskip = 100)




test_list <- list()
for(i in 1:55){
  test$product <- i
  test_list[[i]] <- test
  }
test.df <- do.call(rbind,test_list)
test.df$pred <- mb$prob.test.mean

test_list <- list()
for(i in 1:55){
  print(i)
test_list[[i]] <- test.df[test.df[,paste0("product",i)] == 1,] %>% filter(product == i)
}

test.df <- do.call(rbind,test_list) %>% 
  arrange(across(starts_with("product"))) %>% 
  mutate(id = rep(c(1:2), each = 3)) %>% #why 36? check...
  group_by(id) %>%
  mutate(pred_stand = pred/sum(pred)) %>% ungroup %>%
  select(!(price1:price5))


#merge on attributes using the product as key

test.df2 <- merge(test.df, distinct, all.x = TRUE, all.y=FALSE, by = "product") %>%
  mutate(otter_percent = case_when(otter_25 == 1 ~ 5,
                                  otter_50 == 1 ~ 10,
                                  otter_75 == 1 ~ 15,
                                  TRUE ~ 0),
         whale_percent = case_when(whale_50 == 1 ~ 50,
                                   whale_75 == 1 ~ 75,
                                   whale_100 == 1 ~ 100,
                                  TRUE ~ 0))


ggplot(data =test.df2) +
  geom_boxplot(aes(x = otter_percent, y = pred_stand, group = otter_percent))

ggplot(data =test.df2) +
  geom_boxplot(aes(x = whale_percent, y = pred_stand, group = whale_percent))

ggplot(data =test.df2) +
  geom_boxplot(aes(x = cost, y = pred_stand, group = cost))

ggplot(data =test.df2 ) +
  geom_point(aes(x = cost, y = pred_stand, group = cost)) +
  facet_grid(otter_percent~whale_percent)


