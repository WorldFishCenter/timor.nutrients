render_docs <- function(path = "docs_book") {
  bookdown::render_book(path)

  docs_folder <- system.file("docs_book/docs", package = "Timor.nutrients")
  path_new <- paste0(getwd(), "/docs")
  file.rename(docs_folder, path_new)
}
