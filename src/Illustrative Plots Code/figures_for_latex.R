library(tidyverse)
library(gridExtra)

set.seed(2329580)
starts <- c(.05, rnorm(n = 5, mean = .05, sd = .05))
set.seed(2392580)
ends <- c(.95, rnorm(n = 5, mean = .95, sd = .3))
starts[2] <- .22 

reps <- data.frame(ystarts = starts,
           yends = ends,
           xstarts = rep(0,6),
           xends = rep(.95,6)) %>% 
  mutate(slope = (yends - ystarts)/(.95-0),
           intercept = ystarts,
           r = 1:6 %>% as.character)

r1 <- 5
r2 <- 2

p1 <- ggplot() + 
 # geom_abline(aes(intercept = 0, slope = 1)) +
  scale_x_continuous(limits = c(0, 1.1), breaks = c(.35, .45), labels = c(expression(A[j]), expression(A[j + 1])))+
  scale_y_continuous( breaks = c(reps$slope[r1]*.35 + reps$intercept[r1], reps$slope[r1]*.45 + reps$intercept[r1]), labels = c(expression(F[j]), expression(F[j + 1]))) + 
  geom_segment(aes(x = xstarts, xend = xends, y = ystarts, yend = yends, colour = r), data = reps)+
  theme_bw(base_size = 25) +
  theme(legend.position = "none", panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  geom_segment(aes(x = .35, y = slope*.35 + intercept, xend = .0, yend = slope*.35 + intercept, colour = r), linetype = 2, data = reps[r1,]) +
  geom_segment(aes(x = .35, y = slope*.35 + intercept, xend = .35, yend = 0, colour = r), linetype = 2, data = reps[r1,])+
  geom_segment(aes(x = .45, y = slope*.45 + intercept, xend = .0, yend = slope*.45 + intercept, colour = r), linetype = 2, data = reps[r1,]) +
  geom_segment(aes(x = .45, y = slope*.45 + intercept, xend = .45, yend = 0, colour = r), linetype = 2, data = reps[r1,]) +
  labs(y = "P(Y = 0) = P(No)", x = "") +
  geom_text(aes(x = .95, y = ends, label = paste0("r = ", r), colour = r), hjust = -.2,data = reps, size = 8)

p2 <-ggplot() + 
  # geom_abline(aes(intercept = 0, slope = 1)) +
  scale_x_continuous(limits = c(0, 1.1), breaks = c(.35, .45), labels = c(expression(A[j]), expression(A[j + 1])))+
  scale_y_continuous( breaks = c(reps$slope[r2]*.35 + reps$intercept[r2], reps$slope[r2]*.45 + reps$intercept[r2]), labels = c(expression(F[j]), expression(F[j + 1]))) + 
  geom_segment(aes(x = xstarts, xend = xends, y = ystarts, yend = yends, colour = r), data = reps)+
  theme_bw(base_size = 25) +
  theme(legend.position = "none", panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  geom_segment(aes(x = .35, y = slope*.35 + intercept, xend = .0, yend = slope*.35 + intercept, colour = r), linetype = 2, data = reps[r2,]) +
  geom_segment(aes(x = .35, y = slope*.35 + intercept, xend = .35, yend = 0, colour = r), linetype = 2, data = reps[r2,])+
  geom_segment(aes(x = .45, y = slope*.45 + intercept, xend = .0, yend = slope*.45 + intercept, colour = r), linetype = 2, data = reps[r2,]) +
  geom_segment(aes(x = .45, y = slope*.45 + intercept, xend = .45, yend = 0, colour = r), linetype = 2, data = reps[r2,]) +
  labs(y = "P(Y = 0) = P(No)", x = "") +
  geom_text(aes(x = .95, y = ends, label = paste0("r = ", r), colour = r), hjust = -.2,data = reps, size = 8)

p3 <- grid.arrange(p1, p2, ncol = 2)

ggsave("FA_illustration.pdf",plot = p3, width = 15, height = 10)




ggplot() + 
  # geom_abline(aes(intercept = 0, slope = 1)) +
  scale_x_continuous(limits = c(0, 1.1), breaks = c(.35, .45), labels = c(expression(A[j]), expression(A[j + 1])))+
  scale_y_continuous( breaks = c(.35, .45), labels = c(expression(F[j]), expression(F[j + 1]))) + 
  geom_segment(aes(x = rep(.05, 1), xend = rep(.95, 1), y = starts, yend = ends), data = subset(reps, r == "1"))+
  geom_segment(aes(x = .35, y = .35, xend = .0, yend = .35), linetype = 2, data = subset(reps, r == "4")) +
  geom_segment(aes(x = .35, y = .35, xend = .35, yend = 0), linetype = 2, data = subset(reps, r == "4"))+
  geom_segment(aes(x = .45, y = .45, xend = .0, yend = .45), linetype = 2, data = subset(reps, r == "4")) +
  geom_segment(aes(x = .45, y = .45, xend = .45, yend = 0), linetype = 2, data = subset(reps, r == "4")) +
  theme_bw() +
  theme_bw(base_size = 25)+
  theme(legend.position = "none", panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  labs(y = "P(Y = 0) = P(No)", x = "") 
ggsave("Turn_illustration.pdf")



#############################################

library(tidyverse)
library(gridExtra)



reps <- data.frame(r = factor(rep(1:6, each = 10)),
                   A = rep(1:10, 6)) %>% 
  group_by(r) %>% 
  mutate(FF = cumsum(abs(rnorm(mean = .05, sd = .05, n = 10))))


d1 <- reps[reps$r == 1 & reps$A %in% c(2,4),]
p1 <- ggplot() + 
  # geom_abline(aes(intercept = 0, slope = 1)) +
  scale_x_continuous(breaks = c(2,4),labels = c(expression(A[j]), expression(A[j + 1])), limits = c(0, 12))+
  scale_y_continuous( breaks = d1$FF, labels = c(expression(F[j]), expression(F[j + 1]))) + 
  theme_bw(base_size = 25) +
  theme(legend.position = "none", panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  geom_line(aes(x = A, y = FF, colour = r), data = reps)+
  geom_segment(aes(x =0 , xend = A, y = FF, yend = FF, colour = r), data = reps[reps$r == 1 & reps$A %in% c(2,4),]) +
  geom_segment(aes(x =A , xend = A, y = 0, yend = FF, colour = r), data = reps[reps$r == 1 & reps$A %in% c(2,4),]) +
  labs(y = "P(Y = 0) = P(No)", x = "") +
  geom_text(aes(x = 9, y = FF, label = paste0("r = ", r), colour = r), hjust = -.2,data = reps[reps$A == 10,], size = 8)

d2 <- reps[reps$r == 2 & reps$A %in% c(2,4),]
p2 <- ggplot() + 
  # geom_abline(aes(intercept = 0, slope = 1)) +
  scale_x_continuous(breaks = c(2,4),labels = c(expression(A[j]), expression(A[j + 1])), limits = c(0, 12))+
  scale_y_continuous( breaks = d2$FF, labels = c(expression(F[j]), expression(F[j + 1]))) + 
  theme_bw(base_size = 25) +
  theme(legend.position = "none", panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  geom_line(aes(x = A, y = FF, colour = r), data = reps)+
  geom_segment(aes(x =0 , xend = A, y = FF, yend = FF, colour = r), data = reps[reps$r == 2 & reps$A %in% c(2,4),]) +
  geom_segment(aes(x =A , xend = A, y = 0, yend = FF, colour = r), data = reps[reps$r == 2 & reps$A %in% c(2,4),]) +
  labs(y = "P(Y = 0) = P(No)", x = "") +
  geom_text(aes(x = 9, y = FF, label = paste0("r = ", r), colour = r), hjust = -.2,data = reps[reps$A == 10,], size = 8)

p3 <- grid.arrange(p1, p2, ncol = 2)

ggsave("FA_illustration.pdf",plot = p3, width = 15, height = 10)




ggplot() + 
  # geom_abline(aes(intercept = 0, slope = 1)) +
  scale_x_continuous(limits = c(0, 1.1), breaks = c(.35, .45), labels = c(expression(A[j]), expression(A[j + 1])))+
  scale_y_continuous( breaks = c(.35, .45), labels = c(expression(F[j]), expression(F[j + 1]))) + 
  geom_segment(aes(x = rep(.05, 1), xend = rep(.95, 1), y = starts, yend = ends), data = subset(reps, r == "1"))+
  geom_segment(aes(x = .35, y = .35, xend = .0, yend = .35), linetype = 2, data = subset(reps, r == "4")) +
  geom_segment(aes(x = .35, y = .35, xend = .35, yend = 0), linetype = 2, data = subset(reps, r == "4"))+
  geom_segment(aes(x = .45, y = .45, xend = .0, yend = .45), linetype = 2, data = subset(reps, r == "4")) +
  geom_segment(aes(x = .45, y = .45, xend = .45, yend = 0), linetype = 2, data = subset(reps, r == "4")) +
  theme_bw() +
  theme_bw(base_size = 25)+
  theme(legend.position = "none", panel.grid.minor = element_blank(),panel.grid.major = element_blank())+
  labs(y = "P(Y = 0) = P(No)", x = "") 
ggsave("Turn_illustration.pdf")

