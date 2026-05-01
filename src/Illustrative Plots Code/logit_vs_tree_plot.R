# Load necessary libraries
library(ggplot2)
library(rpart)  # For decision tree
library(rpart.plot)  # For decision tree plot
library(dplyr)
library(broom)  # For tidying model predictions

# Set seed for reproducibility
set.seed(123)

# Simulate dataset with linear and nonlinear interactions
n <- 500
x1 <- runif(n, -10, 10)
x2 <- runif(n, -10, 10)
x3 <- sample(c(0, 1), n, replace = TRUE)  # Binary predictor

# Create nonlinear interaction for the response variable
# Use both linear and nonlinear terms to highlight model differences
y_prob <- plogis(0.3 * x1 + 0.6 * x2^2 - 0.5 * x1 * x2 + 0.7 * x3)
y <- rbinom(n, 1, y_prob)  # Simulated binary outcome

# Put data into a data frame
data <- data.frame(x1 = x1, x2 = x2, x3 = factor(x3), y = y)

# Fit logistic regression model
logit_model <- glm(y ~ x1 + x2 + x3, data = data, family = binomial)

# Fit decision tree model
tree_model <- rpart(y ~ x1 + x2 + x3, data = data, method = "class")
my_palette <- colorRampPalette(c("#A6611A", "white", "#018571"))
# Plot the tree with a custom color palette
rpart.plot(
  tree_model,
  box.palette = my_palette(100), # Create a gradient with 100 colors
extra = 8
  )
# Predict probabilities using both models
data$logit_pred <- predict(logit_model, type = "response")
data$tree_pred <- predict(tree_model)[, 2]  # Probability of y = 1 for decision tree


# Calculate shared range
color_limits <- range(c(data$logit_pred, data$tree_pred), na.rm = TRUE)

# Logistic Regression Predictions
logit_plot <- ggplot(data, aes(x = x1, y = x2, color = logit_pred)) +
  geom_point(size = 2, alpha = 0.6) +
  scale_color_gradient2(
    low = "#A6611A", mid = "white", high = "#018571", midpoint = 0.5, 
    limits = color_limits
  ) +
  labs(title = "Probit Predictions",
       color = "P(y = 1)") +
  theme_minimal()

# Decision Tree Predictions
tree_plot <- ggplot(data, aes(x = x1, y = x2, color = tree_pred)) +
  geom_point(size = 2, alpha = 0.6) +
  scale_color_gradient2(
    low = "#A6611A", mid = "white", high = "#018571", midpoint = 0.5, 
    limits = color_limits
  ) +
  labs(title = "Classification Tree Predictions",
       color = "P(y = 1)") +
  theme_minimal()


# Arrange the plots side by side
library(gridExtra)
p <- grid.arrange(logit_plot, tree_plot, ncol = 2)
p
ggsave("tree_vs_lr.pdf", plot = p)
