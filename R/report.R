#' Render report
#' 
#' Render PDF report from a brew template
#' 
#' @param file brew file
#' @param output output file
#' @param theme theme (default: tufte)
#' @param config_file config file (requires tex template, default: NULL)
#' @param partials whisker partials
#' @param partials_path partials directory, as default it points to "partials" in the same directory as the main script
#' @param data whisker data
#' @param clean remove all temporarily created files (default: TRUE)
#' @examples
#' \dontrun{
#'   render_report(system.file("examples/report-1.template", package = "reportTemplate"), "report-1.pdf")
#'   render_report(system.file("examples/report-2.template", package = "reportTemplate"), "report-2.pdf")
#'   render_report(system.file("examples/report-3.template", package = "reportTemplate"), "report-3.pdf", data = list(title = "This is a plot!"), partials = list(something = "this the standard partial (title = {{title}})"))
#' }
#' @export
render_report <- function(
  file,
  output = "output.pdf",
  theme = "tufte",
  config_file = NULL,
  partials = NULL,
  partials_path = "partials",
  data = NULL,
  clean = T
) {
  
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
  
  # whisker -> brew (support for partials)
  #   Note: There is already support for partial files in whisker,
  #   however they require the partials to be in the current work dir
  #   and for some reason I cannot change the work dir temporarily (=> env error)
  brew <- render_template(
    file,
    data = data,
    partials = c(partials, get_partials(file.path(dirname(file), partials_path)))
  )
  
  # brew -> tex
  Pandoc.brew(
    text = brew,
    output = tex_file,
    convert = 'tex',
    options = paste0('--template=', file.path(tmp_dir, "templates", basename(tmpl))),
    open = F
  )
  
  # Replace code in tex
  tex <- readLines(tex_file)
  for(x in config$tex$replace) {
    tex <- gsub(names(x), pure_value(x), tex)
  }
  cat(tex, file = tex_file, sep = "\n")
  
  #file.copy(tex_file, "output.tex", overwrite = T)
  
  # tex -> pdf
  system(sprintf("pdflatex -output-directory=%s %s ", tmp_dir, tex_file))
  file.copy(file.path(tmp_dir, "output.pdf"), output, overwrite = T)

  
  
  # Clean up
  if (clean == T) {
    unlink(tmp_dir, recursive = T)
  } else {
    message("Temporary files: ", tmp_dir)
  }
}
