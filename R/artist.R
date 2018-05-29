
get_discogs_artist <- function(artist_id, access_token=discogs_api_token()) {

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

  # how many collection pages?
  pages <- data$pagination$pages


  # ITERATION -----------------------------------

  # iterate through pages of collection
  artist_discogs <- purrr::map_dfr(seq_len(pages), function(x){

    # request collection page
    req <- httr::GET(url = paste0(url, "&page=", 1))

    # break if user doesnt exist
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
                                  release_label=label,
                                  artist_role=role,
                                  release_year=year,
                                  release_artist=artist,
                                  release_type=type,
                                  release_id=id,
                                  release_main_id=main_release)
    })

  return(tibble::as_tibble(artist_discogs))

}
