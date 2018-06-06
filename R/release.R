#' Get metadata for a Discogs Release
#'
#' Return metadata for a Release (a particular physical or digital object
#' released by one or more Artists) listed on Discogs.
#'
#' @param release_id The ID of the Release.
#'
#' @param mkt_currency Currency for marketplace data. Defaults to GBP (must be one of:
#'  "GBP", "USD", "EUR", "CAD", "AUD", "JPY", "CHF", "MXN", "BRL", "NZD", "SEK", "ZAR").
#'
#' @param access_token Discogs personal access token, defaults to \code{discogs_api_token}.
#'
#' @return a \code{discogs_release} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @export
#' @examples \dontrun{
#' discogs_release(release_id = 240007)
#' }
discogs_release <- function(release_id, mkt_currency=c("GBP", "USD", "EUR", "CAD", "AUD",
                                                       "JPY", "CHF", "MXN", "BRL", "NZD",
                                                       "SEK", "ZAR"),
                            access_token=discogs_api_token()) {

  # evaluate currency choice
  currency <- match.arg(mkt_currency)

  # check for internet
  check_internet()


  # API REQUEST ---------------------------------------

  # create path
  path <- paste0("releases/", release_id)

  # base API users URL
  url <- httr::modify_url(base_url, path = path)

  # request API for user collection
  req <- httr::GET(url = url, ua,
                   httr::add_headers(Authorization=paste0("Discogs token=", access_token))
                   )

  # break if release doesnt exist
  check_status(req)

  # break if object isnt json
  check_type(req)


  # EXTRACT DATA --------------------------------------

  # extract request content
  data <- jsonlite::fromJSON(httr::content(req, "text", encoding = "UTF-8"),
                             simplifyVector = FALSE)

  # create s3 object
  structure(
    list(
      content = data,
      path = path,
      response = req
    ),
    class = "discogs_database"
  )
}
