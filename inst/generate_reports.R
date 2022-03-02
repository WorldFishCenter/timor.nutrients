
rmarkdown::render(
  input = system.file("plots_report.Rmd", package = "Timor.nutrients"),
  output_dir = system.file(package = "Timor.nutrients"),
  output_format = "all",
  clean=FALSE
)

