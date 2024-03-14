library(gutenbergr)
library(readr)

download_blue_goose <- function(gutenberg_id = 31485, file_name = "blue_goose.csv") {
  blue_goose <- gutenberg_download(gutenberg_id = gutenberg_id,
                                   mirror = "https://gutenberg.pglaf.org/")
  write_csv(blue_goose, file_name)
  return(invisible(TRUE))
}
