library(tidyverse)
library(gridExtra)
set.seed(123)

x <- rnorm(40)

p1 <- data.frame(x) %>%
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 3,
                 col = "white",
                 fill = "#047C91") +
  theme_minimal() +
  ggtitle("3 bins")

p2 <- data.frame(x) %>%
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 10,
                 col = "white",
                 fill = "#047C91") +
  theme_minimal() +
  ggtitle("10 bins")

p3 <- data.frame(x) %>%
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 20,
                 col = "white",
                 fill = "#047C91") +
  theme_minimal() +
  ggtitle("20 bins")


grid.arrange(p1, p2, p3, ncol = 3)
