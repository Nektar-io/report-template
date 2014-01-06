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





### TODO: move to package format.tables? 
### TODO: rethink how to get the parameter type/convert
#' pander.format.tables
#' 
#' function for rendering tables of class format.tables
#' 
#' @param x format table object
#' @export
pander.format.tables <- function(x){
  type <- "html"
  for(i in 1:100){
    if(exists("convert", envir = parent.frame(n=i))){
      type <- get("convert", envir = parent.frame(n=i))
      break
    } 
    if(environmentName(globalenv()) == environmentName(parent.frame(n=i)))
      break
  }
  if(is.logical(type)){
    type <- "html"
  }
  cat(x$render(type=ifelse(type == "pdf", "tex", type)))
}