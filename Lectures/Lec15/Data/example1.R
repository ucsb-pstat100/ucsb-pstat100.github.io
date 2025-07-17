library(tidyverse)
set.seed(123)

x <- rnorm(100, 0, 1)
y <- 0.75 * x^3 + rnorm(100, 0, 1)

plot(x, y)

lin_mod2 <- lm(y ~ poly(x, 3))

data.frame(x = fitted(lin_mod2), y = resid(lin_mod2)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 3) +
  theme_minimal(base_size = 24) +
  xlab("fitted values") + ylab("residuals") +
  ggtitle("Residuals Plot")