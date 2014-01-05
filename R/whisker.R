#' Get partials
#' 
#' Whisker function that list all partials and reads their content.
#' Internal function used by `renderReport()`.
#' 
#' @param path partial folder path
get_partials <- function(path) {
  partials <- list.files(path, full.names = T)
  lst <- lapply(partials, read_file)
  names(lst) <- basename(partials)
  return(lst)
}

#' Render template
#' 
#' Convert template to Rmd.
#' Internal function used by `renderReport()`.
#' 
#' @param file template file
#' @param ... arguments passed to `whisker.render()`
render_template <- function(file, ...) {
  text <- whisker.render(
    template = read_file(file),
    ...
  )
  return(text)
}
