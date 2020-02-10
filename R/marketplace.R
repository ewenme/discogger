#' Get price suggestions for a Discogs release
#'
#' Return price suggestions for a release (a particular physical or digital object
#' released by one or more artists) listed on Discogs.
#'
#' @param release_id The ID of the Release.
#'
#' @return a \code{discogs_database} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @export
#' @examples \dontrun{
#' discogs_marketplace_price_suggestions(release_id = 240007)
#' }
discogs_marketplace_price_suggestions <- function(release_id,
                                                  access_token = discogs_api_token()) {

  # check for internet
  check_internet()

  # create path
  path <- glue("/marketplace/price_suggestions/{release_id}")

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
