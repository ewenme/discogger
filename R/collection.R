#' Get metadata for a user's Discogs collection
#'
#' Return tidy metadata for releases in a user's Discogs collection.
#'
#' @param user_name The username of the collection you are trying to request.
#'
#' @param folder_id The ID of the collection folder (default value is 0,
#' the “All” folder).
#'
#' @param access_token Discogs personal access token, defaults to \code{discogs_api_token}.
#'
#' @return a tibble
#'
#' @export
get_discogs_collection <- function(user_name, folder_id=0, access_token=discogs_api_token()) {

  # URL ---------------------------------------

  # API URL
  url <- paste0("https://api.discogs.com//users/", user_name,
                "/collection/folders/", folder_id,
                "/releases?sort=added&sort_order=desc")


  # API ----------------------------------------------

  # request API for user collection
  req <- httr::GET(url = url)

  # break if user doesnt exist
  httr::stop_for_status(req)

  # extract request content
  data <- httr::content(req)

  # how many collection pages?
  pages <- data$pagination$pages


  # ITERATION -----------------------------------

  # iterate through pages of collection
  collection <- purrr::map_dfr(seq_len(pages), function(x){

    # request collection page
    req <- httr::GET(url = paste0(url, "&page=", x))

    # break if user doesnt exist
    httr::stop_for_status(req)

    # extract request content
    data <- httr::content(req)

    # iterate through pages releases
    release_info <- purrr::map_df(1:length(data$releases), function(x) {

      # top level fields
      release <- data[["releases"]][[x]]

      # basic info fields
      info <- data[["releases"]][[x]]$basic_information

      # label fields
      labs <- unlist(data[["releases"]][[x]]$basic_information$labels)

      # artist fields
      artists <- unlist(data[["releases"]][[x]]$basic_information$artists)

      # format fields
      formats <- unlist(data[["releases"]][[x]]$basic_information$formats)

      # format description fields
      format_descs <- unlist(data[["releases"]][[x]]$basic_information$formats[[1]]$descriptions)

      # create list of fields to keep
      list(
        instance_id = release$instance_id,
        instance_date_added = release$date_added,
        instance_rating = release$rating,
        release_id = release$id,
        release_title = info$title,
        release_year = info$year,
        artist_name = artists[['name']],
        artist_id = artists[['id']],
        label_name = labs[['name']],
        label_cat_no = labs[['catno']],
        label_id = labs[['id']],
        format_name = formats[['name']],
        format_qty = formats[['qty']],
        format_descriptions = list(format_descs)
      )
    })
  })

  # get dates to correct format
  collection$instance_date_added <- lubridate::ymd_hms(collection$instance_date_added)

  # replace zero ratings (not real zero) with NA
  collection$instance_rating[collection$instance_rating == 0] <- NA

  return(tibble::as_tibble(collection))

}
