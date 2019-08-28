#' Slightly more human-readable output for discogs_database objects
#'
#' @param x a discogs_database object
#' @param ... ignored
#' @export
print.discogs_database <- function(x, ...) {

  cat("<Discogs ", x$path, ">\n", sep = "")
  utils::str(x$content)
  invisible(x)

}

#' Slightly more human-readable output for discogs_collection objects
#'
#' @param x a discogs_collection object
#' @param ... ignored
#' @export
print.discogs_collection <- function(x, ...) {

  cat("<Discogs ", x$path, ">\n", sep = "")
  cat("Preview: 1 of", length(x$content), "records.", "\n", sep=" ")
  utils::str(utils::head(x$content, 1))
  invisible(x)

}
