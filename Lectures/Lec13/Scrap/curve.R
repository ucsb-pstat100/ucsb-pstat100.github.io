data.frame(x = -3:3) %>%
  ggplot(aes(x = x)) +
  stat_function(fun = dnorm, linewidth = 1, col = "#4285f4") +
  theme_void()
