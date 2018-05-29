#' Get metadata for a Discogs Artist's Releases
#'
#' Return tidy metadata for an Artist's (a person who contributed
#' to a Release, in some capacity) Releases listed on Discogs.
#'
#' @param artist_id The ID of the Artist.
#'
#' @param access_token Discogs personal access token, defaults to \code{discogs_api_token}.
#'
#' @return a tibble
#'
#' @export
get_discogs_artist_releases <- function(artist_id, access_token=discogs_api_token()) {

  # URL ---------------------------------------

  # artist_id = 1564482

  # base API users URL
  url <- paste0("https://api.discogs.com/artists/", artist_id, "/releases?")


  # API ----------------------------------------------

  # request API for user collection
  req <- httr::GET(url = url)

  # break if artist doesnt exist
  httr::stop_for_status(req)

  # extract request content
  data <- httr::content(req)

  # how many pages?
  pages <- data$pagination$pages


  # ITERATION -----------------------------------

  # iterate through pages
  artist_discogs <- purrr::map_dfr(seq_len(pages), function(x){

    # request artist page
    req <- httr::GET(url = paste0(url, "page=", x))

    # break if artist doesnt exist
    httr::stop_for_status(req)

    # extract request content
    data <- httr::content(req)

    # bind releases
    release_info <- dplyr::bind_rows(data$releases)

    # sleect/rename fields
    release_info <- dplyr::select(release_info,
                                  release_status=status,
                                  release_format=format,
                                  release_title=title,
                                  label_name=label,
                                  artist_role=role,
                                  release_year=year,
                                  artist_name=artist,
                                  release_type=type,
                                  release_id=id,
                                  release_main_id=main_release)
    })

  return(tibble::as_tibble(artist_discogs))

}