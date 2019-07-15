#' Get or set DISCOGS_API_TOKEN value
#'
#' The API wrapper functions in this package all rely on a Discogs access token
#' residing in the environment variable \code{DISCOGS_API_TOKEN}. The
#' easiest way to accomplish this is to set it in the `\code{.Renviron}` file in your
#' home directory.
#'
#' @param force force setting a new Discogs API token for the current environment?
#'
#' @return atomic character vector containing the Discogs API token
#'
#' @export
discogs_api_token <- function(force = FALSE) {

  env <- Sys.getenv('DISCOGS_API_TOKEN')
  if (!identical(env, "") && !force) return(env)

  if (!interactive()) {
    stop("Please set env var DISCOGS_API_TOKEN to your Discogs API access token",
         call. = FALSE)
  }

  message("Couldn't find env var DISCOGS_API_TOKEN See ?discogs_api_token for more details.")
  message("Please enter your Discogs access token and press enter:")
  pat <- readline(": ")

  if (identical(pat, "")) {
    stop("Discogs API access token entry failed", call. = FALSE)
  }

  message("Updating DISCOGS_API_TOKEN env var to PAT")
  Sys.setenv(DISCOGS_API_TOKEN = pat)

  pat

}
