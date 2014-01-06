\name{render_report}
\alias{render_report}
\title{Render report}
\usage{
render_report(file, output = "output.pdf", theme = "tufte",
  config_file = NULL, partials = NULL, partials_path = "partials",
  data = NULL, format = "tex", tex_engine = "pdflatex",
  tex_options = "-halt-on-error", tex_runs = 2, keep_tex = FALSE)
}
\arguments{
  \item{file}{brew file}

  \item{output}{output file}

  \item{theme}{theme (default: tufte)}

  \item{config_file}{config file (requires tex template,
  default: NULL)}

  \item{partials}{whisker partials}

  \item{partials_path}{partials directory, as default it
  points to "partials" in the same directory as the main
  script}

  \item{data}{whisker data}

  \item{format}{format of the report, takes "tex" or
  "html"}

  \item{tex_engine}{which tex engine should bee used
  (default: pdflatex)}

  \item{tex_options}{options passed to the tex engine}

  \item{tex_runs}{how many times should tex run, e.g. for
  updating table of contents}

  \item{keep_tex}{should the tex file (as raw.tex) be
  copied to the ouput directory (default: FALSE)}
}
\description{
Render PDF report from a brew template
}
\examples{
\dontrun{
  render_report(system.file("examples/report-1.template", package = "reportTemplate"), "report-1.pdf")
  render_report(system.file("examples/report-2.template", package = "reportTemplate"), "report-2.pdf")
  render_report(system.file("examples/report-3.template", package = "reportTemplate"), "report-3.pdf", data = list(title = "This is a plot!"), partials = list(something = "this the standard partial (title = {{title}})"))
}
}

