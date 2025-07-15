library(tidyverse)
library(gridExtra)
set.seed(123)

x <- rnorm(10)

density(x, bw = "nrd0", kernel = "rectangular")

data.frame(x) %>%
  ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 13,
                 col = "white") +
  stat_density(geom = "line",
               kernel = "gaussian",
               col = "blue",
               linewidth = 1,
               bw = 0.1)



p1 <- data.frame(x) %>%
  ggplot(aes(x = x)) +
  geom_point(aes(x = x, y= rep(0, 10))) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 13,
                 col = "white") +
  stat_density(geom = "line",
               kernel = "rectangular",
               col = "blue",
               linewidth = 1,
               bw = 0.01) +
  ylab("density") +
  ggtitle("Locally Binned Histogram; Binwidth 0.01") +
  theme_minimal()

p2 <- data.frame(x) %>%
  ggplot(aes(x = x)) +
  geom_point(aes(x = x, y= rep(0, 10))) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 13,
                 col = "white") +
  stat_density(geom = "line",
               kernel = "rectangular",
               col = "blue",
               linewidth = 1,
               bw = 0.38) +
  ylab("density") +
  ggtitle("Locally Binned Histogram; Binwidth 0.38") +
  theme_minimal()

p3 <- data.frame(x) %>%
  ggplot(aes(x = x)) +
  geom_point(aes(x = x, y= rep(0, 10))) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 13,
                 col = "white") +
  stat_density(geom = "line",
               kernel = "rectangular",
               col = "blue",
               linewidth = 1,
               bw = 1) +
  ylab("density") +
  ggtitle("Locally Binned Histogram; Binwidth 1") +
  theme_minimal()


## Gaussian KDE

data.frame(x, y = rep(0, length(x))) %>%
  ggplot(aes(x = x)) +
  geom_point(aes(x = x, y = y)) +
  stat_density(
    geom = "line",
    kernel = "gaussian",
    linewidth = 1,
    bw = 0.38,
    col = "blue"
  )

grid.arrange(p1, p2, p3, ncol = 1)




## 

epan <- Vectorize(function(x) {return((3/4) * (1 - x^2)) * ifelse(((x > -1) & (x < 1)), x, 0)})
triang <- Vectorize(function(x) {return(1 - abs(x)) * ifelse(((x > -1) & (x < 1)), x, 0)})
biweight <- Vectorize(function(x) {return((35 / 32) * (1 - x^2)^3) * ifelse(((x > -1) & (x < 1)), x, 0)})
cosine2 <- Vectorize(function(x) {return((pi / 4) * cos((pi / 2) * x)) * ifelse(((x > -1) & (x < 1)), x, 0)})

data.frame(x = -1:1) %>%
  ggplot(aes(x = x)) +
  stat_function(fun = epan,
                aes(col = "Epanechnikov"),
                linewidth = 1) +
  stat_function(fun = triang,
                aes(col = "Triangular"),
                linewidth = 1) +
  stat_function(fun = biweight,
                aes(col = "Biweight"),
                linewidth = 1) +
  stat_function(fun = cosine2,
                aes(col = "Cosine"),
                linewidth = 1) +
  theme_minimal(base_size = 12) +
  labs(colour = "Legend") +
  ggtitle("A Few Different Kernels")


####
library(tidyverse)
set.seed(123)


x <- runif(100, 0, 1)
y <- 2 * x^3 - 2 * x^2 + 0.1 * x + rnorm(100, 0, 0.05)

data.frame(x, y) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point()


library(gplm)
kreg(x, y)


data.frame(x = c(x, rep(NA, 300)), y = c(y, rep(NA, 300)),
           z = as.vector(kreg(x, y)$x),
           w =  as.vector(kreg(x, y)$y)) %>%
  ggplot() +
  geom_point(aes(x = x, y = y)) +
  geom_line(aes(x = z,
                y = w),
            col = "blue",
            linewidth = 1) +
  theme_minimal() +
  ggtitle("Biweight Kernel Regression, 0.17 bandwidth")
