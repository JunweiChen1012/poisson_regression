count_of_e_simulation <-
  tibble(
    chapter = c(rep(1, 10), rep(2, 10), rep(3, 10)),
    line = rep(1:10, 3),
    number_words_in_line = runif(min = 0, max = 15, n = 30) |> round(0),
    number_e = rpois(n = 30, lambda = 10)
  )

count_of_e_simulation |>
  ggplot(aes(y = number_e, x = number_words_in_line)) +
  geom_point() +
  labs(
    x = "Number of words in line",
    y = "Number of n/Ns in the first ten lines"
  ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set1")

