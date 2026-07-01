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
set.seed(10)
weights <- c(0.25, rep(0.75 / 5, 5))
rolls_200 <- sample(1:6, 200, replace = TRUE, prob = weights)

rolls_200 %>%
  tibble(face = factor(.)) %>%
  count(face) %>%
  ggplot(aes(x = face, y = n)) +
  geom_col(fill = "steelblue", color = "black") +
  labs(x = "Face", y = "Count", title = "Loaded Die: Frequency of Each Face in 200 Rolls") +
  theme_minimal()
#
#
#
#
#
