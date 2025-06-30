library(tidyverse)
library(plotly)

data.frame(var1 = c(1, 2, 3), var2 = c(2, 3, 4), 
           ind = c(1, 2, 3)) %>%
  plot_ly(
    x = ~var1,
    y = ~var2,
    text = ~paste("Individual", ind, 
                  "<br>Variable 1:", var1,
                  "<br>Variable 2:", var2)
  ) %>%
  add_markers(
    marker = list(size = 20)
  ) %>%
  layout(
    title = list(
      text = "Row-Wise Viewpoint",
      font = list(size = 24)
    ),
    margin = list(t = 75, b = 75)
  )




data.frame(obs1 = c(1, 2), obs2 = c(2, 3), obs3 = c(3, 4), 
           ind = c(1, 2)) %>%
  plot_ly(
    x = ~obs1,
    y = ~obs2,
    z = ~obs3,
    text = ~paste("Variable", ind, 
                  "<br>Observation 1:", obs1,
                  "<br>Observation 2:", obs2,
                  "<br>Observation 3:", obs3)
  ) %>%
  add_markers(
    marker = list(size = 20)
  ) %>%
  layout(
    title = list(
      text = "Column-Wise Viewpoint",
      font = list(size = 24)
    ),
    margin = list(t = 75, b = 75)
  )
