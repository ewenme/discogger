#' Get metadata for a Discogs Artist
#'
#' Return tidy metadata for an Artist (a person who contributed
#' to a Release, in some capacity) listed on Discogs.
#'
#' @param artist_id The ID of the Artist.
#'
#' @param access_token Discogs personal access token, defaults to \code{discogs_api_token}.
#'
#' @return a tibble
#'
#' @export
get_discogs_artist <- function(artist_id, access_token=discogs_api_token()) {

  # check for internet
  check_internet()

  # URL ---------------------------------------

  # base API users URL
  url <- paste0(base_url, "artists/", artist_id)


  # API ----------------------------------------------

  # request API for user collection
  req <- httr::GET(url = url)

  # break if artist doesnt exist
  check_status(req)

  # extract request content
  data <- httr::content(req)


  # DATA ----------------------------------------------

  # put artist fields into df format
  artist <- tibble::tibble(
    artist_profile = data$profile,
    artist_name = data$name,
    artist_id = data$id,
    artist_name_variations = data$namevariations,
    artist_urls = list(data$urls),
    artist_aliases = data$aliases,
    artist_data_quality = data$data_quality,
    artist_real_name = data$realname
  )

  return(artist)

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
#' @return a tibble
#'
#' @export
get_discogs_artist_releases <- function(artist_id, access_token=discogs_api_token()) {

  # check for internet
  check_internet()

  # URL ---------------------------------------

  # base API users URL
  url <- paste0(base_url, "artists/", artist_id, "/releases?")


  # API ----------------------------------------------

  # request API for user collection
  req <- httr::GET(url = url)

  # break if artist doesnt exist
  check_status(req)

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

  # add artist id
  artist_discogs <- dplyr::mutate(artist_discogs,
                                  artist_id=artist_id)

  return(tibble::as_tibble(artist_discogs))

}
