#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
#
#
#
#| cache: true
throw_dice <- function(n = 1, weights = rep(1/6, 6)) {
  d1 <- sample(1:6, n, replace = TRUE, prob = weights)
  d2 <- sample(1:6, n, replace = TRUE, prob = weights)
  d1 + d2
}
#
#
#
theoretical_pdf <- bind_rows(
  tibble(face = factor(1:6), probability = rep(1/6, 6), die = "Fair"),
  tibble(face = factor(1:6), probability = c(0.30, rep(0.14, 5)), die = "Loaded")
)

theoretical_pdf %>%
  ggplot(aes(x = face, y = probability, fill = die)) +
  geom_col(color = "black") +
  facet_wrap(~ die, nrow = 1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.35)) +
  labs(x = "Face", y = "Probability", title = "Theoretical PDFs: Fair vs. Loaded Die") +
  theme_minimal() +
  theme(legend.position = "none")
#
#
#
#| cache: true
log_lr <- function(rolls) {
  p_loaded <- ifelse(rolls == 6, 0.30, 0.14)
  p_fair <- 1/6
  log(p_loaded / p_fair)
}
#
#
#
set.seed(10)
rolls_200 <- sample(1:6, 200, replace = TRUE, prob = c(0.14, 0.14, 0.14, 0.14, 0.14, 0.30))

gamma <- cumsum(log_lr(rolls_200))
posterior <- 1 / (1 + exp(-gamma))

data.frame(
  roll = seq_along(posterior),
  posterior = posterior
) %>%
  ggplot(aes(x = roll, y = posterior)) +
  geom_point(size = 1.5, color = "steelblue") +
  geom_line(color = "steelblue", alpha = 0.6) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
  labs(
    x = "Roll number",
    y = "Posterior probability loaded",
    title = "Running posterior for loaded die after each roll"
  ) +
  theme_minimal()
#
#
#
#
#
