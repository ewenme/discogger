#' Get metadata for a Discogs artist
#'
#' Return metadata for an artist (a person who contributed
#' to a Release, in some capacity) listed on Discogs.
#'
#' @param artist_id The ID of the artist.
#' @param access_token Discogs personal access token, defaults to \code{discogs_api_token}.
#'
#' @return a \code{discogs_database} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @export
#' @examples \dontrun{
#' discogs_artist(artist_id = 36314)
#' }
discogs_artist <- function(artist_id, access_token = discogs_api_token()) {

  check_internet()

  path <- glue("artists/{artist_id}")

  url <- modify_url(base_url, path = path)

  # request API for artist info
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

  # return s3 object
  structure(
    list(
      content = data,
      path = path,
      response = req
    ),
    class = "discogs_database"
  )
}

#' Get metadata for a Discogs artist's releases
#'
#' Return tidy metadata for an artist's (a person who contributed
#' to a Release, in some capacity) releases listed on Discogs.
#'
#' @inheritParams discogs_artist
#'
#' @return a \code{discogs_database} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @export
#' @examples \dontrun{
#' discogs_artist_releases(artist_id = 36314)
#' }
discogs_artist_releases <- function(artist_id, access_token = discogs_api_token()) {

  check_internet()

  path <- glue("artists/{artist_id}/releases?")

  url <- modify_url(base_url, path = path)

  # request API for artist
  req <- discogs_get(
    url = url, ua, add_headers(
      Authorization = glue("Discogs token={access_token}")
      )
    )

  check_status(req)
  check_type(req)

  # extract request content
  data <- fromJSON(
    content(req, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
    )

  # how many pages?
  pages <- data$pagination$pages

  # iterate through pages
  artist_discogs <- lapply(seq_len(pages), function(x){

    # request artist page
    req <- discogs_get(
      url = paste0(url, "page=", x), ua,
      add_headers(Authorization = glue("Discogs token={access_token}")
                  )
      )

    stop_for_status(req)
    check_type(req)

    # extract request content
    data <- fromJSON(
      content(req, "text", encoding = "UTF-8"),
      simplifyVector = TRUE, flatten = TRUE
      )

    # bind releases
    rbind.data.frame(data$releases)

    })

  artist_discogs <- do.call("rbind", artist_discogs)

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
