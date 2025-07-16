library(tidyverse)
set.seed(123)

x <- rnorm(100, 68, 4)
hist(x)

y <- 30 + 0.5*x + rnorm(100, 2)

plot(x, y)

data.frame(temp = x, aqi = y) %>%
  write.csv("aqi.csv", row.names = FALSE)