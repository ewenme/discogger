#' Get metadata for a Discogs Label's Releases
#'
#' Return tidy metadata for a Label's (a label, company, recording studio, location,
#' or other entity involved with Artists and Releases) Releases listed on Discogs.
#'
#' @param label_id The ID of the Label.
#'
#' @param access_token Discogs personal access token, defaults to \code{discogs_api_token}.
#'
#' @return a tibble
#'
#' @export
get_discogs_label_releases <- function(label_id, access_token=discogs_api_token()) {

  # base API users URL
  url <- paste0("https://api.discogs.com/labels/", label_id, "/releases?")

  # API ----------------------------------------------

  # request API for user collection
  req <- httr::GET(url = url)

  # break if release doesnt exist
  httr::stop_for_status(req)

  # extract request content
  data <- httr::content(req)

  # how many pages?
  pages <- data$pagination$pages


  # ITERATION -----------------------------------

  # iterate through pages
  label_discogs <- purrr::map_dfr(seq_len(pages), function(x){

    # request label page
    req <- httr::GET(url = paste0(url, "page=", x))

    # break if label doesnt exist
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
                                  label_cat_no=catno,
                                  release_year=year,
                                  artist_name=artist,
                                  release_id=id)
  })

  return(tibble::as_tibble(label_discogs))

  }
