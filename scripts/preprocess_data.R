library(tidyverse)
library(stringr)

preprocess_blue_goose <- function(file_name = "blue_goose.csv") {
  blue_goose <- read_csv(file_name, col_types = cols(gutenberg_id = col_integer(), text = col_character()))
  
  blue_goose_reduced <- blue_goose |>
    filter(!is.na(text)) |>
    mutate(chapter = if_else(str_detect(text, "CHAPTER") == TRUE,
                             text,
                             NA_character_)) |>
    fill(chapter, .direction = "down") |>
    mutate(chapter_line = row_number(), 
           .by = chapter) |>
    filter(!is.na(chapter), 
           chapter_line %in% c(2:30)) |>
    select(text, chapter) |>
    mutate(
      chapter = str_remove(chapter, "CHAPTER "),
      chapter = str_remove(chapter, "—CONCLUSION"),
      chapter = as.integer(as.roman(chapter))
    ) |>
    mutate(count_n = str_count(text, "n|N"),
           word_count = str_count(text, "\\w+")
    )
  
  return(blue_goose_reduced)
}
