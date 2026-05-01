############################################################
## Linked visuals:
##   Top:    Pr(Yes | A, x) vs x (color = bid A)
##   Bottom: WTP distribution over x (heatmap) with bid thresholds
##            - Probit: latent Normal WTP|x
##            - BART-style: implied density from survival S(w|x)=Pr(Yes|w,x)
############################################################

library(tidyverse)
library(patchwork)

set.seed(1)

############################################################
## Settings
############################################################
x_grid  <- seq(0, 1, length.out = 200)     # covariate x
A_lines <- c(10, 25, 50, 75, 100)          # bid thresholds shown as dashed lines
w_grid  <- seq(0, 140, length.out = 400)   # WTP axis for densities

############################################################
## 1) PROBIT interpretation: WTP|x ~ Normal(mu(x), sd) where mu(x) is a linear function of x
##    Pr(Yes | A, x) = Pr(WTP >= A | x) = 1 - Phi((A - mu(x))/sd)
############################################################
mu_probit <- function(x) 30 + 60 * x
sd_probit <- 18

# Probability curves implied by latent-normal WTP|x
probit_prob_df <- expand_grid(x = x_grid, A = A_lines) %>%
  mutate(
    p_yes = 1 - pnorm((A - mu_probit(x)) / sd_probit),
    model = "Probit",
    A_f = factor(A, levels = sort(unique(A)))
  )

# Heatmap density f(w|x)
probit_density <- expand_grid(x = x_grid, w = w_grid) %>%
  mutate(
    mu = mu_probit(x),
    f  = dnorm(w, mean = mu, sd = sd_probit),
    model = "Probit"
  )

############################################################
## 2) BART-style acceptance surface: flexible in x, A with interaction
##    We'll interpret S(w|x)=Pr(Yes|w,x) as survival, then f(w|x) = -dS/dw
############################################################
p_yes_bart_like <- function(A, x) {
  
  # scale: big around some x -> less dispersion across A
  #        small around other x -> more dispersion across A
  s <- 1 + 1.2 * exp(-((x - 0.25)/0.15)^2)
  # if you want the "compressed" region somewhere else, move 0.25
  
  eta <- (
    3.0
    + 2.2 * x
    - 0.05 * A                 # flip sign
    - 0.0018 * A^2             # keep negative (more decrease at high A)
    - 0.17 * A * (x - 0.5)     # flip sign so A effect stays decreasing
    + 3.5 * sin(2*pi * x) 
  )
  
  plogis(eta / s)
}

# Probability curves (the ones you want on top-right)
bart_prob_df <- expand_grid(x = x_grid, A = A_lines) %>%
  mutate(
    p_yes = p_yes_bart_like(A, x),
    model = "BART",
    A_f = factor(A, levels = sort(unique(A)))
  )

# Build survival S(w|x) on w_grid
bart_survival <- expand_grid(x = x_grid, w = w_grid) %>%
  mutate(S = p_yes_bart_like(w, x)) %>%
  group_by(x) %>%
  arrange(w, .by_group = TRUE) %>%
  # enforce monotone decreasing in w (valid survival curve)
  mutate(S = cummin(S)) %>%
  ungroup()

# Recover implied density by finite differences
dw <- diff(w_grid)[1]
bart_density <- bart_survival %>%
  group_by(x) %>%
  arrange(w, .by_group = TRUE) %>%
  mutate(f = pmax((S - lead(S)) / dw, 0)) %>%
  ungroup() %>%
  filter(!is.na(f)) %>%
  mutate(model = "BART")

############################################################
## 3) Probability plots: x on x-axis, p on y-axis, color = bid A
############################################################
p_prob_probit <- ggplot(probit_prob_df, aes(x = x, y = p_yes, color = A_f, group = A_f)) +
  geom_line(linewidth = 1.1) +
  labs(
    # title = "Probit: Pr(Yes | A, x) as a function of x",
    x = "Covariate (x)",
    y = "Pr(Yes | A, x)"
  ) +
  theme_minimal(base_size = 13) +
  scale_colour_brewer("Bid (A)")
p_prob_probit
p_prob_bart <- ggplot(bart_prob_df, aes(x = x, y = p_yes, color = A_f, group = A_f)) +
  geom_line(linewidth = 1.1) +
  labs(
    # title = "BART-style: Pr(Yes | A, x) as a function of x",
    x = "Covariate (x)",
    y = "Pr(Yes | A, x)"
  ) +
  theme_minimal(base_size = 13) +
  scale_colour_brewer("Bid (A)")
p_prob_bart
############################################################
## 4) Heatmaps: x on x-axis, WTP on y-axis; dashed lines are bids
##    Tail above WTP=A corresponds to Pr(Yes | A, x)
############################################################
p_wtp_probit <- ggplot(probit_density, aes(x = x, y = w, fill = f)) +
  geom_raster(interpolate = TRUE) +
  geom_hline(yintercept = A_lines, linetype = "dashed", linewidth = 0.6) +
  labs(
    title = "Probit latent-WTP view",
    x = "Covariate (x)",
    y = "WTP (latent)",
    fill = "density"
  ) +
  theme_minimal(base_size = 13) +  scale_fill_gradient(
    low = "white",
    high = "black"
  ) 

#Right-tail area above WTP = A equals Pr(Yes | A, x)
p_wtp_bart <- ggplot(bart_density, aes(x = x, y = w, fill = f)) +
  geom_raster(interpolate = TRUE) +
  geom_hline(yintercept = A_lines, linetype = "dashed", linewidth = 0.6) +
  labs(
    title = "BART latent-WTP distribution",
    x = "Covariate (x)",
    y = "WTP (latent)",
    fill = "density"
  ) +
  theme_minimal(base_size = 13)+
  theme_minimal(base_size = 13) +  scale_fill_gradient(
    low = "white",
    high = "black"
  ) 

############################################################
## 5) One 2×2 linked figure
############################################################
final_fig <-
  (p_prob_probit | p_prob_bart) /
  (p_wtp_probit  | p_wtp_bart) +
  plot_annotation(
    title = "Connecting acceptance probabilities to WTP distributions: Probit vs BART"
  )
#"Top: Pr(Yes | A, x) curves. Bottom: WTP|x distributions where tail above bid thresholds equals Pr(Yes | A, x)."
print(final_fig)
ggsave(plot = final_fig, filename = "connecting_prob_to_wtp.pdf", width = 12, height = 12)


