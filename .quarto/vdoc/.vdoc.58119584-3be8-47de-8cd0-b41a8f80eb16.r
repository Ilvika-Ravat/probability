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
set.seed(10)
roll_sizes <- c(10, 20, 50, 100, 1000)
rolls <- tibble(
  n = rep(roll_sizes, times = roll_sizes),
  sum = unlist(lapply(roll_sizes, function(n) throw_dice(n)))
)

rolls %>%
  ggplot(aes(x = sum)) +
  geom_histogram(breaks = seq(1.5, 12.5, by = 1), fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = 2:12, limits = c(1.5, 12.5)) +
  facet_wrap(~ n, nrow = 1, scales = "free_y") +
  labs(x = "Sum", y = "Count", title = "Two-dice throw histograms as sample size grows") +
  theme_minimal()
#
#
#
#
