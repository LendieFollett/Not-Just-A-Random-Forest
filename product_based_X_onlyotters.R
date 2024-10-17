rm(list = ls())
library(tidyverse)
library(BART)
d <- read.csv("int03_BART_5Jul2023.csv")
str(d)

d <- d %>% 
  mutate(otter_low = 1- (otter_25 + otter_50 + otter_75),
         otter_med = otter_25 + otter_50,
         otter_high = otter_75) %>%
  select(-c(otter_25, otter_50 ,otter_75))
keeps <- c("cost", "otter_low", "otter_med" ,"otter_high")
distinct <- d[!duplicated(d[,keeps]),c("grp", keeps)] 
distinct <- distinct %>%
  mutate(product = factor(as.integer(factor(paste0(cost, otter_low, otter_med, otter_high)))),
         ref = paste(cost, otter_low, otter_med, otter_high, sep = "_"))
head(distinct)
d <- d %>%
  mutate(product = factor(as.integer(factor(paste0(cost, otter_low, otter_med, otter_high)))),
         ref = paste(cost, otter_low, otter_med, otter_high, sep = "_"))

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

colnames(d_price) <- c("grp", paste0("price", 1:16))
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
for(i in 1:16){
  test$product <- i
  test_list[[i]] <- test
}
test.df <- do.call(rbind,test_list)
test.df$pred <- mb$prob.test.mean

test_list <- list()
for(i in 1:16){
  test_list[[i]] <- test.df[test.df[,paste0("product",i)] == 1,] %>% filter(product == i)
  test_list[[i]]$price <- test_list[[i]][,paste0("price",i)]
  
}

test.df <- do.call(rbind,test_list) %>% 
  arrange(across(starts_with("product"))) %>% 
  mutate(id = rep(c(1:32), each = 3)) %>% #why 36? check...
  group_by(id) %>%
  mutate(pred_stand = pred/sum(pred)) %>% 
  ungroup 


#merge on attributes using the product as key

test.df2 <- merge(test.df, distinct, all.x = TRUE, all.y=FALSE, by = "product") %>%
  mutate(otter_percent = case_when(otter_low == 1 ~ "0",
                                   otter_med == 1 ~ "25-50",
                                   otter_high == 1 ~ "75"))


ggplot(data =test.df2) +
  geom_boxplot(aes(x = otter_percent, y = pred_stand, group = otter_percent))

ggplot(data =test.df2) +
  geom_boxplot(aes(x = cost, y = pred_stand, group = cost))



#product = combinations of price and attributes. 

table(test.df2$otter_percent, test.df2$price)

test.df2 <- test.df2 %>% select(otter_percent, price, pred)

b_util <- mean(filter(test.df2, otter_percent == "0" & price == 0)$pred) #baseline utility

temp <- filter(test.df2, otter_percent == "25-50")
which.min(abs(b_util-temp$pred))
temp[10,] #$150

temp <- filter(test.df2, otter_percent == "75")
which.min(abs(b_util-temp$pred))
temp[17,] #$300

ggplot() + 
  geom_point(aes(x = price, y = pred), data = temp) +
  geom_hline(aes(yintercept = temp[17,"pred"]))
#horizontal line shows utility of SQ
