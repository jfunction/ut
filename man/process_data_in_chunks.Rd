% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_data_in_chunks.R
\name{process_data_in_chunks}
\alias{process_data_in_chunks}
\title{Process delimited file in chunks}
\usage{
process_data_in_chunks(
  filename,
  chunk_size = 10000,
  function_on_chunk = identity,
  function_to_combine = rbind,
  verbose = FALSE,
  n_lines = NULL,
  ...
)
}
\arguments{
\item{filename}{Name of the delimited file to read.}

\item{chunk_size}{Number of lines per chunk.}

\item{function_on_chunk}{A callback function to call on each chunk.}

\item{function_to_combine}{Function to combine results from chunks.}

\item{verbose}{Whether to be explicit about that is happening.}

\item{n_lines}{Total number of lines to read. If NULL (default) it will try to guess.}

\item{...}{other options passed into \link[vroom:vroom]{vroom::vroom}.}
}
\value{
The bound dataframe.
}
\description{
Uses \link{vroom} to process delimited file in chunks,  performing a function
on each chunk then using (potentially another) function to combine the
processed chunks. Not in any way fully tested...
}
