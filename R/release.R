#' Get metadata for a Discogs release
#'
#' Return metadata for a release (a particular physical or digital object
#' released by one or more artists) listed on Discogs.
#'
#' @param release_id The ID of the Release.
#' @param mkt_currency Currency for marketplace data. Defaults to GBP (must be one of:
#'  "GBP", "USD", "EUR", "CAD", "AUD", "JPY", "CHF", "MXN", "BRL", "NZD", "SEK", "ZAR").
#' @inheritParams discogs_artist
#'
#' @return a \code{discogs_database} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @export
#' @examples \dontrun{
#' discogs_release(release_id = 240007)
#' }
discogs_release <- function(release_id,
                            mkt_currency = c(
                              "GBP", "USD", "EUR", "CAD", "AUD",
                              "JPY", "CHF", "MXN", "BRL", "NZD",
                              "SEK", "ZAR"
                              ),
                            access_token = discogs_api_token()) {

  # evaluate currency choice
  currency <- match.arg(mkt_currency)

  # check for internet
  check_internet()

  # create path
  path <- glue("releases/{release_id}")

  # base API users URL
  url <- modify_url(base_url, path = path)

  # request API for user collection
  req <- discogs_get(
    url = url, ua,
    add_headers(Authorization = glue("Discogs token={access_token}")
                )
    )

  check_status(req)
  check_type(req)

  # extract request content
  data <- fromJSON(
    content(req, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
    )

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

#' Get metadata for a Discogs master release
#'
#' Return metadata for a master release (a set of similar Releases) listed
#' on Discogs.
#'
#' @param master_id The Master ID of a release
#' @inheritParams discogs_artist
#'
#' @return a \code{discogs_database} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @export
#' @examples \dontrun{
#' discogs_release_master(master_id = 684265)
#' }
discogs_release_master <- function(master_id, access_token = discogs_api_token()) {

  # check for internet
  check_internet()

  # create path
  path <- glue("masters/{master_id}")

  # base API users URL
  url <- modify_url(base_url, path = path)

  # request API for user collection
  req <- discogs_get(
    url = url, ua,
    add_headers(Authorization = glue("Discogs token={access_token}")
    )
  )

  check_status(req)
  check_type(req)

  # extract request content
  data <- fromJSON(
    content(req, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )

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
