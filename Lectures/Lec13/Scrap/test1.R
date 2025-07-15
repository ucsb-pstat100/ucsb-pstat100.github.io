library(tidyverse)
set.seed(123)

x <- rnorm(25, 2)
y <- 0.5 * x + rnorm(25, 0, 1)

data.frame(x = x, y = y) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point(size = 2) +
  theme_minimal() +
  geom_smooth(formula = y ~ poly(x, 7),
              method = "lm",
              se = F,
              col = "blue",
              linewidth = 1)
