render_docs <- function(path = "docs_book") {
  bookdown::render_book(path)
 #bookdown::render_book("docs_book", "bookdown::pdf_book")

  docs_folder <- system.file("docs_book/docs", package = "timor.nutrients")
  path_new <- paste0(getwd(), "/docs")
  file.rename(docs_folder, path_new)
}
