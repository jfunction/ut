# Motivated from https://stackoverflow.com/a/23456450
# Additionally accounts for possibly many /r/n or /n at the
# end of the file
get_n_lines <- function(filename) {
  f <- file(filename,
            open="rb")
  on.exit(close(f))

  fileSize <- file.size(filename)
  nlines <- 0L
  while (length(chunk <- readBin(f, "raw", 64*(1024)^1)) > 0) {  # 64Kb means approx 16,000 chunks per Gb
    nlines <- nlines + sum(chunk == as.raw(10L))
  }
  # We still need to handle the lines at the end
  i <- 1
  numEndLinefeeds <- 0
  seek(f,fileSize-i)
  while ((nxtChar<-readBin(f, "raw", 1)) %in% c(as.raw(10L),as.raw(13L))) {
    if (nxtChar==as.raw(10L)) numEndLinefeeds <- numEndLinefeeds + 1
    i = i+1
    seek(f, fileSize-i)
  }
  headerRows <- 1
  nrows = nlines + 1  # linefeed at end of row not including anything at the very end
  (nrows - headerRows) - numEndLinefeeds
}

#' Process delimited file in chunks
#'
#' Uses [vroom] to process delimited file in chunks,  performing a function
#' on each chunk then using (potentially another) function to combine the
#' processed chunks. Not in any way fully tested...
#'
#' @param filename Name of the delimited file to read.
#' @param chunk_size Number of lines per chunk.
#' @param function_on_chunk A callback function to call on each chunk.
#' @param function_to_combine Function to combine results from chunks.
#' @param verbose Whether to be explicit about that is happening.
#' @param n_lines Total number of lines to read. If NULL (default) it will try to guess.
#' @param ... other options passed into [vroom::vroom()].
#'
#' @returns The bound [data.frame], [tibble], or something else if you modified `function_to_combine`.
#'
#' @seealso [vroom::vroom()] which this function wraps.
#' @export
process_data_in_chunks <- function(filename, chunk_size=10000,
                                   function_on_chunk=identity,
                                   function_to_combine=rbind,
                                   verbose=FALSE,
                                   n_lines=NULL,
                                   ...) {
  # TODO: consider (show_col_types = FALSE) ?
  if(is.null(n_lines)) {
    if(verbose)
      print("Getting total number of lines in file...")
    n_lines <- get_n_lines(filename)
  }

  if(verbose)
    print(paste0("There are ", n_lines, " lines in file."))
  n_chunks <- ceiling(n_lines / chunk_size)

  if(verbose)
    print("Pulling out chunks and processing them.")
  data <- vector(mode="list")
  cnames <- colnames(vroom::vroom(filename, n_max=0,
                                  show_col_types = FALSE,
                                  .name_repair = ))
  lsResult <- lapply(seq(n_chunks), function(i) {
    if(verbose)
      print(paste0("Processing chunk ", i, "/", n_chunks, "..."))
    tmp <- vroom::vroom(filename, n_max=chunk_size,
                        skip = (i - 1) * chunk_size + 1,
                        col_names = cnames, show_col_types = FALSE,
                        ...)

    # if (csize<=0) browser()

    function_on_chunk(tmp)
  })
  do.call(what=function_to_combine, args=lsResult)
}
