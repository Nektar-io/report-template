reportTemplate
==============

**WORK IN PROGRESS**

## Requirements

- (Mac OS) install X11 support, e.g. XQuartz-2.7.5.dmg
- Pandoc
- pdflatex

## Examples

    renderReport(system.file("examples/report.brew", package = "reportTemplate"))
    
## Todo

- Add support for custom pander class functions (see [Pander S3](https://github.com/Rapporter/pander/blob/master/R/S3.R))
- Add support for whisker partials (see [knitr-whisker-template](https://bitbucket.org/reinholdsson/knitr-whisker-template/src))

## Inspiration

Many thanks to:

- [ggplotFL](https://github.com/flr/ggplotFL) by Laurie T. Kell and Iago Mosqueira
- [pander](https://github.com/Rapporter/pander) by Gergely Daróczi
- [slidify](https://github.com/ramnathv/slidify) by Ramnath Vaidyanathan
