#' Return only pure value
#' 
#' Function removes unnecessary attributes and returns pure value
#' 
#' @param x input value
pure_value <- function(x) {
  names(x) <- NULL
  return(unlist(x))
}

#' Package file
#' 
#' Internal package function that returns package file path
#' 
#' @param x file name
#' @param path folder path
pkg_file <- function(x, path = NULL) {
  if (!is.null(path)) x <- file.path(path, x)
  system.file(x, package = getPackageName())
}

#' Read file
#' 
#' Read file and return as a string
#' 
#' @param file
read_file <- function(file) {
  con <- file(file)
  txt <- paste(readLines(con, warn = F), collapse = "\n")
  txt <- paste(txt, "\n", sep = "")
  close(con)
  return(txt)
}
