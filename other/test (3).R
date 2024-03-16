library(testthat)
library(ggplot2)
library(modelsummary)
test_that("No NA values in text", {
  expect_true(all(!is.na(blue_goose_reduced$text)))
})

test_that("Chapters correctly identified", {
  expect_true(all(!is.na(blue_goose_reduced$chapter)))
})

test_that("Chapter line numbers are correct", {
  expect_true(all(blue_goose_reduced$chapter_line >= 2 & blue_goose_reduced$chapter_line <= 30))
})

test_that("Chapters are correctly converted to integers", {
  expect_type(blue_goose_reduced$chapter, "integer")
  expect_true(all(blue_goose_reduced$chapter > 0))
})

test_that("Counts of 'n/N's and words are correct", {
  known_values <- data.frame(text = c("An example sentence.", "Another one."),
                             count_n = c(3, 2), 
                             word_count = c(3, 2)) 
  for (i in 1:nrow(known_values)) {
    row <- known_values[i, ]
    filtered <- blue_goose_reduced[blue_goose_reduced$text == row$text, ]
    expect_equal(filtered$count_n, row$count_n)
    expect_equal(filtered$word_count, row$word_count)
  }
})

test_that("Mean and variance are calculated correctly", {
  calculated_mean <- mean(blue_goose_reduced$count_n)
  calculated_variance <- var(blue_goose_reduced$count_n)
  expect_equal(mean_n, calculated_mean)
  expect_equal(variance_n, calculated_variance)
})

test_that("Histogram and scatter plot are created without errors", {
  expect_error(
    blue_goose_reduced %>%
      ggplot(aes(x = count_n)) +
      geom_histogram() +
      geom_vline(xintercept = mean_n, linetype = "dashed", color = "#C64191") +
      geom_vline(xintercept = variance_n, linetype = "dashed", color = "#0ABAB5"),
    NA
  )
  expect_error(
    blue_goose_reduced %>%
      ggplot(aes(x = word_count, y = count_n)) +
      geom_jitter(alpha = 0.5) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed"),
    NA
  )
})

test_that("stan_glm fits without errors and returns correct object", {
  model_fit <- stan_glm(
    count_n ~ word_count,
    data = blue_goose_reduced,
    family = poisson(link = "log"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 853,
    refresh = 0
  )
  expect_is(model_fit, "stanreg")
  expect_true("count_n" %in% names(coefficients(model_fit)))
  expect_true("word_count" %in% names(coefficients(model_fit)))
})

test_that("Model is saved correctly", {
  saveRDS(bluegoose_n_counts, file = "bluegoose_n_counts.rds")
  expect_true(file.exists("bluegoose_n_counts.rds"))
})

test_that("Model summary is produced without errors", {
  summary_output <- modelsummary(list("Number of n/Ns" = bluegoose_n_counts))
  expect_is(summary_output, "modelsummary_msummary")
})

test_that("Predictions plot is generated without errors", {
  plot_output <- plot_predictions(bluegoose_n_counts, condition = "word_count")
  expect_is(plot_output, "gg")
})

test_dir("test_dir("project/tests/")
