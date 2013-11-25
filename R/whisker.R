# Read file to string
read_file <- function(file) {
  con <- file(file)
  txt <- paste(readLines(con, warn = F), collapse = "\n")
  txt <- paste(txt, "\n", sep = "")
  close(con)
  return(txt)
}

# List partials and read their content
get_partials <- function(path) {
  partials <- list.files(path, full.names = T)
  lst <- lapply(partials, read_file)
  names(lst) <- basename(partials)
  return(lst)
}

# Convert template to Rmd
render_template <- function(file, ...) {
  text <- whisker.render(
    template = read_file(file),
    ...
  )
  return(text)
}
