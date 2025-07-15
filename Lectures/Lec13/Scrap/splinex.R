library(tidyverse)
library(gridExtra)

set.seed(123)
n <- 100

x <- runif(n, 0, 10)
y <- cos((pi / 4)*x) + rnorm(n, 0, 0.5)

p1 <- data.frame(x = x, y = y) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  theme_minimal() +
  geom_smooth(
    formula = y ~ bs(x, df = 3, knots = c(2.5, 5, 7.5)),
    method = "lm",
    se = F
  ) +
  geom_vline(xintercept = 2.5,
             linetype = "dotted") +
  geom_vline(xintercept = 5,
             linetype = "dotted")+
  geom_vline(xintercept = 7.5,
             linetype = "dotted") +
  ggtitle("Cubic Regression Spline with 3 Knots")



p2 <- data.frame(x = x, y = y) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  theme_minimal() +
  geom_smooth(
    formula = y ~ bs(x, df = 3, knots = c(0.5, 1, 9)),
    method = "lm",
    se = F
  ) +
  geom_vline(xintercept = 0.5,
             linetype = "dotted") +
  geom_vline(xintercept = 1,
             linetype = "dotted")+
  geom_vline(xintercept = 9,
             linetype = "dotted") +
  ggtitle("Cubic Regression Spline with 3 Knots")


grid.arrange(p1, p2, ncol = 1)