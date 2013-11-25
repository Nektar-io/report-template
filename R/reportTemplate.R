#' Return only pure value
pure_value <- function(x) {
  names(x) <- NULL
  return(unlist(x))
}

#' Package file
pkg_file <- function(x, path = NULL) {
  if (!is.null(path)) x <- file.path(path, x)
  system.file(x, package = getPackageName())
}

#' Render report
#' 
#' Render PDF report from a brew template
#' 
#' @param file brew file
#' @param output output file
#' @oaram theme theme (default: tufte)
#' @param config_file config file (requires tex template, default: NULL)
#' @examples
#' \dontrun{
#' renderReport(system.file("examples/report.brew", package = "reportTemplate"))
#' }
#' @export
renderReport <- function(file, output = "output.pdf", theme = "tufte", config_file = NULL) {
  
  # Read config file
  if (is.null(config_file)) {
    config_file <- pkg_file(paste0(theme, ".yml"), "config")
  }
  
  if (file.exists(config_file)) {
    config <- yaml.load_file(config_file)
  } else stop("Config file is missing!")
  
  # Evaluation options
  for(x in config$evalsOptions) {
    evalsOptions(names(x), pure_value(x))
  }
  
  # Pander options
  for(x in config$panderOptions) {
    panderOptions(names(x), pure_value(x))
  }
  
  # Generate temporary file path
  tmp_dir <- dirname(tempfile())
  
  # Create temporary directories
  dir.create(tmp_dir, showWarnings = F)
  dir.create(file.path(tmp_dir, "templates"), showWarnings = F)
  
  # Generate temporary tex file
  tex_file <- file.path(tmp_dir, "output.tex")
  
  file.copy(file, file.path(tmp_dir, basename(file)), overwrite = T)
  
  # Tex template
  tmpl <- config$tex$template
  
  if (is.null(tmpl) || !file.exists(tmpl)) {
    if (file.exists(pkg_file(tmpl, "templates"))) {
      tmpl <- pkg_file(tmpl, "templates")
    } else stop("Tex template is missing!")
  }
  
  file.copy(tmpl, file.path(tmp_dir, "templates", basename(tmpl)), overwrite = T)
  
  # brew -> tex
  Pandoc.brew(
    file = file.path(file),
    output = tex_file,
    convert = 'tex',
    options = paste0('--template=', tmpl),
    open = F
  )
  
  # Replace code in tex
  tex <- readLines(tex_file)
  for(x in config$tex$replace) {
    tex <- gsub(names(x), pure_value(x), tex)
  }
  cat(tex, file = tex_file, sep="\n")
  
  # tex -> pdf
  system(sprintf("pdflatex -output-directory=%s %s ", tmp_dir, tex_file))
  file.copy(file.path(tmp_dir, "output.pdf"), output, overwrite = T)
  
  # Clean up
  unlink(tmp_dir, recursive = T)
}
