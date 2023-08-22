rm(list = ls())
library(devtools)
devtools::install_github("https://github.com/bpkindo/mpbart")
library(mpbart)

p=3
train_wave = mlbench.waveform(300)
test_wave = mlbench.waveform(500)
traindata = data.frame(train_wave$x, y = train_wave$classes) 
testdata = data.frame(test_wave$x, y = test_wave$classes)


sigma0 = diag(p-1)
burn = 100
ndraws <- 200
Mcmc1=list(sigma0=sigma0, burn = burn, ndraws = ndraws)
Prior1 = list(nu=p+2,
              V=(p+2)*diag(p-1),
              ntrees = 100, 
              kfac = 2.0, 
              pbd = 1.0, 
              pb = 0.5, 
              alpha = 0.99,  
              beta =  2.0, 
              nc = 200, 
              priorindep = FALSE)


 out <- mpbart(as.factor(y) ~ 1 | X1 + X2 + X3 + X4 + X5 + X6 + 
                 X7 + X8 + X9 + X11 + X12 + X13 +
                 X14 + X15 + X16 + X17 + X18 + X19 +
                 X20 + X21,
               train.data =  traindata, 
               test.data =  testdata,
               base = NULL, 
               varying = NULL,
               sep = NULL,
               Prior = Prior1, 
               Mcmc = Mcmc1, 
               seedvalue = 99)
 
str(out) 
 
 