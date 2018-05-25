
get_discogs_artist <- function(artist_id, access_token=discogs_api_token()) {

  # URL ---------------------------------------

  artist_id = 1564482

  # base API users URL
  url <- paste0("https://api.discogs.com/artists/", artist_id, "/releases")


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
    req <- httr::GET(url = paste0(req_url, "&page=", x))

    # break if user doesnt exist
    httr::stop_for_status(req)

    # extract request content
    data <- httr::content(req)

    # iterate through pages releases
    release_info <- purrr::map_df(1:length(data$releases), function(x) {

      # top level fields
      release <- data[["releases"]][[x]]

      # create list of fields to keep
      list(
        release_id = release$id,
        release_title = release$title,
        release_year = release$year
      )
    })
  })

  return(tibble::as_tibble(collection))


}
