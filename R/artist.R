#' Get metadata for a Discogs Artist
#'
#' Return metadata for an Artist (a person who contributed
#' to a Release, in some capacity) listed on Discogs.
#'
#' @param artist_id The ID of the Artist.
#'
#' @param access_token Discogs personal access token, defaults to \code{discogs_api_token}.
#'
#' @return a \code{discogs_database} object that contains the extracted content from the request,
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
  url <- modify_url(base_url, path = path)

  # request API for user collection
  req <- discogs_get(url = url)

  # break if artist doesnt exist
  check_status(req)

  # break if object isnt json
  check_type(req)


  # EXTRACT DATA ---------------------------------------

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


#' Get metadata for a Discogs Artist's Releases
#'
#' Return tidy metadata for an Artist's (a person who contributed
#' to a Release, in some capacity) Releases listed on Discogs.
#'
#' @param artist_id The ID of the Artist.
#'
#' @param access_token Discogs personal access token, defaults to \code{discogs_api_token}.
#'
#' @return a \code{discogs_database} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @export
#' @examples \dontrun{
#' discogs_artist_releases(artist_id = 36314)
#' }
discogs_artist_releases <- function(artist_id, access_token=discogs_api_token()) {

  # check for internet
  check_internet()

  # create path
  path <- glue("artists/{artist_id}/releases?")

  # base API users URL
  url <- modify_url(base_url, path = path)

  # request API for artist
  req <- discogs_get(
    url = url, ua, add_headers(
      Authorization = glue("Discogs token={access_token}")
      )
    )

  # break if artist doesnt exist
  check_status(req)

  # break if object isnt json
  check_type(req)

  # extract request content
  data <- fromJSON(
    content(req, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
    )

  # how many pages?
  pages <- data$pagination$pages

  # iterate through pages
  artist_discogs <- map_dfr(seq_len(pages), function(x){

    # request artist page
    req <- discogs_get(
      url = paste0(url, "page=", x), ua,
      add_headers(Authorization = glue("Discogs token={access_token}")
                  )
      )

    # break if artist doesnt exist
    stop_for_status(req)

    # break if object isnt json
    check_type(req)

    # extract request content
    data <- fromJSON(
      content(req, "text", encoding = "UTF-8"),
      simplifyVector = FALSE
      )

    # bind releases
    release_info <- bind_rows(data$releases)

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
    class = "discogs_database"
  )
}
