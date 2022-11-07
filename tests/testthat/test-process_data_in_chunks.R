NROW = 601  # deliberately prime and of form 5*x+1 and 6*x+1
file = tempfile(pattern = 'testData', fileext = '.txt')
withr::with_seed(seed = NROW, code = {
  write.csv(file = file,
            x = data.frame(category=sample(c("A","B","C"),NROW,replace=T),
                           ratio=runif(NROW),
                           ints=seq(NROW)))
})
on.exit(unlink(file))

test_that("process_data_in_chunks", {
  # Shamelessly copied from https://github.com/tidyverse/readr/blob/main/tests/testthat/test-read-chunked.R
  # We just first make our own file instead of store one in package data:

  unchunked <- read.csv(file)

  get_first_int <- function(data) data[1,4]

  # chunk_size > number of lines
  result <- process_data_in_chunks(file = file, function_on_chunk = get_first_int)
  expect_equal(result, tibble::tibble(unchunked)[1,4])

  # Each line separately
  result <- process_data_in_chunks(file = file, function_on_chunk = get_first_int, chunk_size = 1)
  expect_equal(result, tibble::tibble(unchunked)[,4])

  # In chunks of 5
  result <- process_data_in_chunks(file = file, function_on_chunk = get_first_int, chunk_size = 5)
  expect_equal(result, tibble::tibble(unchunked)[seq(1,nrow(unchunked),5),4])
})
