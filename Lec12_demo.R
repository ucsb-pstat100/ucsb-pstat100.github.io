### QQ-Plot Demo
library(tidyverse)

## Start by generating a sample that comes from a normal distribution
x <- rnorm(1000)

## Generate a QQ-Plot using base R
qqnorm(x)    # generates the QQ-Plot
qqline(x)    # adds a line to the graph to help determine linearity

## Generate a QQ-Plot using ggplot
data.frame(x) %>%
  ggplot(aes(sample = x)) +
  geom_qq() +
  geom_qq_line(linewidth = 1,
               col = "blue") +
  theme_minimal() +
  ylab("sample quantiles") +
  xlab("theoretical quantiles")

## when interpreting a QQ-plot, look for deviations from linearty
## especially in the tails of the plot.
## Deviations from linearty imply non-normality.

## If you're curious, here's a sketch of how the base R
## qqnorm() function works behind the scenes:

x_ax <- (ppoints(length(x)) %>% qnorm())[order(order(x))]
y_ax <- x

plot(x_ax, y_ax)
