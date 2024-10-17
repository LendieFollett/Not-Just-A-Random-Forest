# Set seed for reproducibility
set.seed(123 + r)

# Number of observations
n <- 1000

# Setting up parameters
sigma_W <- 35  # standard deviation for normal distribution
beta_N0 <- 100  # for normal distribution
beta_L0 <- log(100) - 0.5 * log(1 + (sigma_W/100)^2)  # for log-normal distribution
beta_B0 <- 100  # for bimodal distribution
beta_U0 <- 100  # for uniform distribution
beta_1 <- 2

a <- 0.5 * sigma_W * sqrt(12)  # for uniform distribution

# X_i drawn from U[-30, 30]
Xi <- runif(n, -20, 20)

#For friedman function
X1 <- runif(n,0,1)
X2 <- runif(n,0,1)
X3 <- runif(n,0,1)
X4 <- runif(n,0,1)
X5 <- runif(n,0,1)

# Normal WTP_i = beta_N0 + beta_1 * X_i + error term (normally distributed)
epsilon_Ni <- rnorm(n, mean = 0, sd = sigma_W)
WTP_normal <- beta_N0 + beta_1 * Xi + epsilon_Ni

# Log-normal WTP_i = exp(beta_L0 + beta_1 * X_i + error term (normally distributed))
sigma_L <- sqrt(log(1 + (sigma_W/100)^2))
epsilon_Li <- rnorm(n, mean = 0, sd = sigma_L)
WTP_lognormal <- exp(-10 + beta_L0 + 1 * Xi + epsilon_Li)

# Bimodal WTP_i = beta_B0 + beta_1 * X_i + error term (normally distributed) + tau_i (bimodal)
epsilon_Bi <- rnorm(n, mean = 0, sd = 1)
rho_i <- runif(n)
tau_i <- ifelse(rho_i > 0.5, 1, -1)
delta <- sqrt(sigma_W^2 - 1)
WTP_bimodal <- beta_B0 + beta_1 * Xi + epsilon_Bi + tau_i * delta

# Uniform WTP_i = beta_U0 + beta_1 * X_i + error term (uniformly distributed)
epsilon_Ui <- runif(n, -a, a)
WTP_uniform <- beta_U0 + beta_1 * Xi + epsilon_Ui

#Friedman
WTP_friedman <- 50*sin(pi*X1*X2) + 100*(X3 - 0.5)^2 + 50*X4 + 05*X5 + rnorm(n, 0, sigma_W)


# Step function, normal error
epsilon_Ni <- rnorm(n, mean = 0, sd = sigma_W)
WTP_step <- beta_N0 + -30 * (Xi < 0) + 30*(Xi > 0) + epsilon_Ni


# Create a data frame to store the results
data <- data.frame(
  Xi = Xi,
  WTP_normal = WTP_normal,
  WTP_lognormal = WTP_lognormal,
  WTP_bimodal = WTP_bimodal,
  WTP_uniform = WTP_uniform,
  WTP_friedman = WTP_friedman
)

# Print first few rows
head(data)
A <- c(25, 50, 75, 125, 175)
A_samps <- sample(A, size = nrow(data), replace=TRUE) %>% as.matrix

survey <- data.frame(Xi = Xi,
                     A = A_samps,
                     apply(data[,-1], 2, function(x){ifelse(x > A_samps, 1, 0)}),
                     X1, X2, X3, X4, X5)

train.idx <- sample(1:nrow(survey), size = .7*nrow(survey), replace=FALSE)

train <- survey[train.idx,]
test <-survey[-train.idx,]
test.wtp <- data[-train.idx,]



