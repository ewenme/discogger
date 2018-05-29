#' Get metadata for a Discogs release
#'
#' Return tidy metadata for a release (a particular physical or digital object
#' released by one or more Artists) listed on Discogs.
#'
#' @param release_id The ID of the release.
#'
#' @param access_token Discogs personal access token, defaults to \code{discogs_api_token}.
#'
#' @return a tibble
#'
#' @export
get_discogs_release <- function(release_id, access_token=discogs_api_token()) {

  # URL ---------------------------------------

  # base API users URL
  url <- paste0("https://api.discogs.com/releases/", release_id)

  # API ----------------------------------------------

  # request API for user collection
  req <- httr::GET(url = url)

  # break if release doesnt exist
  httr::stop_for_status(req)

  # extract request content
  data <- httr::content(req)

  # DATA ----------------------------------------------

  # artist fields
  artist <- unlist(data[["artists"]])

  # community fields
  community <- unlist(data[["community"]])

  # style fields
  style <- unlist(data[["styles"]])

  # genre fields
  genre <- unlist(data[["genres"]])

  # labels fields
  labels <- unlist(data[["labels"]])

  # formats
  formats <- unlist(data[["formats"]][[1]])

  # format descriptions
  format_descs <- unlist(data[["formats"]][[1]][["descriptions"]])

  # create df of fields to keep
  release <- tibble::tibble(
    release_id = data$id,
    release_master_id = data$master_id,
    release_title = data$title,
    release_year = data$year,
    release_country = data$country,
    release_styles = list(style),
    release_genres = list(genre),
    release_notes = data$notes,
    release_num_for_sale = data$num_for_sale,
    release_lowest_price = data$lowest_price,
    release_weight = data$estimated_weight,
    # release_extra_artists = data[["extraartists"]],
    artist_id = artist[['id']],
    artist_name = artist[['name']],
    label_id = labels[['id']],
    label_name = labels[['name']],
    label_cat_no = labels[['catno']],
    community_rating_count = community[['rating.count']],
    community_rating_avg = community[['rating.average']],
    community_have = community[['have']],
    community_want = community[['want']],
    format_name = formats[['name']],
    format_qty = formats[['qty']],
    format_descriptions = list(format_descs)
    )

  # fix numeric fields
  release <- dplyr::mutate_at(release,
                              c("artist_id", "label_id",
                                "community_rating_count",
                                "community_rating_avg",
                                "community_have", "community_want",
                                "format_qty"), as.numeric)

  return(release)

  }
