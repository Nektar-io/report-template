reportTemplate
==============

**WORK IN PROGRESS**

## Requirements

- (Mac OS) install X11 support, e.g. XQuartz-2.7.5.dmg
- Pandoc
- pdflatex

## Examples

    render_report(system.file("examples/report-1.template", package = "reportTemplate"), "report-1.pdf")
    render_report(system.file("examples/report-2.template", package = "reportTemplate"), "report-2.pdf")
    render_report(system.file("examples/report-3.template", package = "reportTemplate"), "report-3.pdf", data = list(title = "This is a plot!"), partials = list(something = "this the standard partial (title = {{title}})"))


## Todo

- Add support for custom pander class functions (see [Pander S3](https://github.com/Rapporter/pander/blob/master/R/S3.R))
- ~~Add support for whisker partials (see [knitr-whisker-template](https://bitbucket.org/reinholdsson/knitr-whisker-template/src))~~ (done!)

## Inspiration

Many thanks to:

- [ggplotFL](https://github.com/flr/ggplotFL) by Laurie T. Kell and Iago Mosqueira
- [pander](https://github.com/Rapporter/pander) by Gergely Daróczi
- [slidify](https://github.com/ramnathv/slidify) by Ramnath Vaidyanathan
