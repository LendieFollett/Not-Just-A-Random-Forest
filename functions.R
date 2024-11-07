
library(Iso)

get_wtp <- function(pred_matrix){
  
  
  test_ends[paste0("Rep", 1:1000)] <- NA
  
  test_ends[paste0("Rep", 1:1000)] <- lapply(test_ends[paste0("Rep", 1:1000)], function(x) {
    ifelse(test_ends$CV_donate_bid == max(pts), 1, 0)
  })
  
  pred_matrix2 <- data.frame(pred_matrix) 
  
  colnames(pred_matrix2) <- gsub("X", "Rep", colnames(pred_matrix2))
  
  testb <- test_notends %>% 
    data.frame(pred_matrix2)#(1-mb$prob.test) %>%  t() 

  
  tdat <-testb %>%
    rbind(test_ends) %>% #tack on endpoints
    group_by(ID) %>% 
    arrange(ID, desc(CV_donate_bid)) %>% 
    # calculate differences in consecutive probabilities
    # (some will be negative, that is taken care of below)
     mutate(across(starts_with("Rep"), ~  pava(.x, decreasing=TRUE))) %>% #Rep = P(said no)
    mutate(across(starts_with("Rep"), ~  lag(.)- ., .names = "diff_{col}")) %>% 
    arrange(ID, CV_donate_bid)
  
  #View(tdat[,c(1,5,1005)])
  
  tdat <- tdat %>% select("ID", "CV_donate_bid",paste0("diff_Rep", 1:1000))
  
  
  tdat_long <- tdat %>% 
    #select(-starts_with("Rep")) %>% 
    melt(id.vars = c("ID", "CV_donate_bid"))# %>% 
  #filter(!is.na(hh_income))
  
  #View(tdat[,c(1:5,1000:1005)])
  
  #tdat_long <- tdat %>% 
  #  select(-starts_with("Rep")) %>% 
   # melt(id.vars = c("CV_donate_bid", "hh_income", "age", "ID")) %>% 
    #filter(!is.na(hh_income))
  
  #calculate weighted sum
  #for negative differences (non-monotonicity), give a value of 0
  #tdat_long2 <- tdat_long%>% 
  #  group_by(hh_income, variable, age) %>% 
  #  summarise(wtp = sum(CV_donate_bid*pmax(value,0), na.rm=TRUE)/sum(pmax(value,0), na.rm=TRUE)) %>% 
  #  ungroup
  
  tdat_long2 <- tdat_long%>% 
    group_by(ID,variable) %>% #hh_income, variable, age
    mutate(CDF = c(1,1-cumsum(value))[-length(value)]) %>%
    summarise(wtp_t = sum(CV_donate_bid*value, na.rm=TRUE),
              wtp_q = integrate(approxfun(CV_donate_bid[!is.na(CDF)],CDF[!is.na(CDF)]), 0, max(CV_donate_bid))$value) 
  
  
  mean_wtp <-tdat_long2 %>% 
    ungroup %>% 
    group_by(ID) %>% 
    summarise(wtp_t = mean(wtp_t),
              wtp_q = mean(wtp_q))
  
return(list(wtp=tdat_long2, mean_wtp))
  
}


get_wtp2 <- function(pred_matrix, test_ends,test_notends){
  pts <- unique(test_ends$A)
  max_pts <- max(pts)
  
  # Create a matrix instead of repeatedly assigning NA columns
  test_ends[paste0("Rep", 1:1000)] <- as.numeric(!test_ends$A == max_pts)
  
  pred_matrix2 <- data.frame(pred_matrix) 
  colnames(pred_matrix2) <- gsub("X", "Rep", colnames(pred_matrix2))
  
  testb <- test_notends %>% 
    data.frame(pred_matrix2)#(1-mb$prob.test) %>%  t() 
  #colnames(testb) <- gsub("X", "Rep", colnames(testb))
  
  
  tdat <-testb %>%
    rbind(test_ends) %>% #tack on endpoints
    group_by(ID) %>% 
    arrange(ID, (A)) %>% 
    mutate(across(starts_with("Rep"), ~  pava(.x, decreasing=TRUE))) %>% 
    ungroup() %>% 
    arrange(ID, A)
  
 # View(tdat[,c(1:3,13,1013)])

  tdat <- tdat %>% select("ID", "A",paste0("Rep", 1:1000))
  

  
  tdat_long <- tdat %>%
    pivot_longer(cols = starts_with("Rep"), names_to = "variable", values_to = "value")
  
  #calculate weighted sum
  #for negative differences (non-monotonicity), give a value of 0
  tdat_long2 <- tdat_long%>% 
    group_by(ID,variable) %>% #hh_income, variable, age
    #mutate(CDF = c(1,1-cumsum(value))[-length(value)]) %>%
    summarise(wtp_q = integrate(approxfun(A[!is.na(value)],value[!is.na(value)]), 0, 150)$value) %>% 
    ungroup %>% 
    group_by(ID) %>% 
    summarise(wtp_q = mean(wtp_q))
  
  return(tdat_long2)
  
}

# 
# temp =tdat_long%>% 
#   group_by(ID,variable) %>% #hh_income, variable, age
#   mutate(CDF = c(1,1-cumsum(value))[-length(value)])
# 
# 
# temp2 <- temp %>% 
#   filter(ID %in% c(3) & variable %in% paste0("diff_Rep",1:5))   
# 
# temp3 <- tdat_long2 %>% filter(ID %in% c(3) & variable %in% paste0("diff_Rep",1:5))
# 
#   ggplot() + 
#     geom_line(aes(x = rep(1:10, 5), group = variable, y = CDF, colour = variable), data = temp2 %>% filter(CV_donate_bid > 0)) +
#     geom_text(aes(x = 10, y = CDF,colour = variable, label = paste0("Area = ", round(temp3$wtp_q,1))),data = temp2 %>% filter(CV_donate_bid ==2000), hjust = .3, size = 6)+
#     theme_bw() +scale_colour_grey() +
#     labs(x="A", y = "P(Y = 1 | A)") +
#     theme(legend.position = "none", axis.text.x = element_blank(),
#           axis.text.y = element_blank(),
#           text = element_text(size = 25)) +
#     expand_limits(x=c(0,11.5))
# ggsave("integral_mcmc.pdf")



get_wtp_point <- function(pred_vector, test_ends,test_notends){
  pts <- unique(test_ends$A)
  test_ends$Rep1 <- NA
  
  test_ends$Rep1 <-ifelse(test_ends$A == max(pts), 1, 0)

  
  testb <- test_notends %>% 
    data.frame(Rep1 = pred_vector)#(1-mb$prob.test) %>%  t() 
  #colnames(testb) <- gsub("X", "Rep", colnames(testb))
  
  
  
  tdat <-testb %>%
    rbind(test_ends) %>% #tack on endpoints
    group_by(ID) %>% 
    arrange(ID, (A)) %>% 
    # calculate differences in consecutive probabilities
    # (some will be negative, that is taken care of below)
    mutate(across(starts_with("Rep"), ~  pava(.x, decreasing=TRUE))) %>% 
    arrange(ID, A)
  
  
  
  
  
  tdat <- tdat %>% select("ID", "A",paste0("Rep1"))
  
  
  
  tdat_long <- tdat %>% 
    #select(-starts_with("Rep")) %>% 
    melt(id.vars = c("ID", "A"))# %>% 
  #filter(!is.na(hh_income))
  
  tdat_long2 <- tdat_long%>% 
    group_by(ID,variable) %>% #hh_income, variable, age
    summarise(wtp_q = integrate(approxfun(A[!is.na(value)],CDF[!is.na(value)]), 0, 150)$value) %>% 
    ungroup %>% 
    group_by(ID) %>% 
    summarise(wtp_q = mean(wtp_q))
  
  #TOMORROW: need to have CDF = 1 for A = 0. (i.e., shift the CDF values down by one row)
  #TOMORROW: check SK computations - why is it biased downward
  # ---> don't use SK. use quadrature instead (which is what SK wants to estimate anyway). 
  #TOMORROW: instead of using pmax, try combining bins when there is non-monotonicity
  
  return(tdat_long2)
  
}
