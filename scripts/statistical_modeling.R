library(rstanarm)
library(ggplot2)
library(tidyverse)

fit_glm_poisson <- function(data) {
  model <- stan_glm(
    count_n ~ word_count,
    data = data,
    family = poisson(link = "log"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 853,
    refresh = 0
  )
  saveRDS(model, file = "bluegoose_n_counts.rds")
  return(model)
}

visualize_predictions <- function(model) {
  plot_predictions(model, condition = "word_count") +
    labs(x = "Number of words",
         y = "Average number of n/Ns in the first 30 lines") +
    theme_classic()
}
