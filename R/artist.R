#' Get metadata for a Discogs Artist
#'
#' Return metadata for an Artist (a person who contributed
#' to a Release, in some capacity) listed on Discogs.
#'
#' @param artist_id The ID of the Artist.
#'
#' @param access_token Discogs personal access token, defaults to \code{discogs_api_token}.
#'
#' @return a \code{discogs_artist} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @export
#' @examples \dontrun{
#' discogs_artist(artist_id = 36314)
#' }
discogs_artist <- function(artist_id, access_token=discogs_api_token()) {

  # check for internet
  check_internet()

  # API REQUEST ---------------------------------------

  # create path
  path <- paste0("artists/", artist_id)

  # base API users URL
  url <- httr::modify_url(base_url, path = path)

  # request API for user collection
  req <- httr::GET(url = url)

  # break if artist doesnt exist
  check_status(req)

  # break if object isnt json
  check_type(req)


  # EXTRACT DATA ---------------------------------------

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
    class = "discogs_artist"
  )
}

print.discogs_artist <- function(x, ...) {

  cat("<Discogs ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)

}

#' Get metadata for a Discogs Artist's Releases
#'
#' Return tidy metadata for an Artist's (a person who contributed
#' to a Release, in some capacity) Releases listed on Discogs.
#'
#' @param artist_id The ID of the Artist.
#'
#' @param access_token Discogs personal access token, defaults to \code{discogs_api_token}.
#'
#' @return a \code{discogs_artist_releases} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @export
#' @examples \dontrun{
#' discogs_artist_releases(artist_id = 36314)
#' }
discogs_artist_releases <- function(artist_id, access_token=discogs_api_token()) {

  # check for internet
  check_internet()

  # API REQUEST ---------------------------------------

  # create path
  path <- paste0("artists/", artist_id, "/releases?")

  # base API users URL
  url <- httr::modify_url(base_url, path = path)

  # request API for artist
  req <- httr::GET(url = url, ua,
                   httr::add_headers(Authorization=paste0("Discogs token=", access_token))
                   )

  # break if artist doesnt exist
  check_status(req)

  # break if object isnt json
  check_type(req)


  # EXTRACT DATA --------------------------------------

  # extract request content
  data <- jsonlite::fromJSON(httr::content(req, "text", encoding = "UTF-8"),
                             simplifyVector = FALSE)

  # how many pages?
  pages <- data$pagination$pages

  # iterate through pages
  artist_discogs <- purrr::map_dfr(seq_len(pages), function(x){

    # request artist page
    req <- httr::GET(url = paste0(url, "page=", x), ua,
                     httr::add_headers(Authorization=paste0("Discogs token=", access_token))
                     )

    # break if artist doesnt exist
    httr::stop_for_status(req)

    # break if object isnt json
    check_type(req)

    # extract request content
    data <- jsonlite::fromJSON(httr::content(req, "text", encoding = "UTF-8"),
                               simplifyVector = FALSE)

    # bind releases
    release_info <- dplyr::bind_rows(data$releases)

    })

  # add artist id
  artist_discogs$artist_id <- artist_id

  # create s3 object
  structure(
    list(
      content = artist_discogs,
      path = path,
      response = req
    ),
    class = "discogs_artist_releases"
  )
}

print.discogs_artist_releases <- function(x, ...) {

  cat("<Discogs ", x$path, ">\n", sep = "")
  x$content
  invisible(x)

}
