#' Get metadata for a Discogs User Collection
#'
#' Return metadata for releases in a Discogs User Collection.
#'
#' @param user_name The username of the Collection you are trying to request.
#'
#' @param folder_id The ID of the Collection folder (default value is 0,
#' the “All” folder).
#'
#' @param simplify_df Coerce list of results into a nested data frame object
#'
#' @param access_token Discogs personal access token, defaults to \code{discogs_api_token}.
#'
#' @return a \code{discogs_collection} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @export
#' @examples \dontrun{
#' discogs_user_collection(user_name = "rodneyfool")
#' }
discogs_user_collection <- function(user_name, folder_id = 0, simplify_df = FALSE,
                                    access_token=discogs_api_token()) {

  # check for internet
  check_internet()

  # API REQUEST ---------------------------------------

  # create path
  path <- glue::glue("users/{user_name}/collection/folders/{folder_id}/releases?sort=added&sort_order=desc")

  # base API users URL
  url <- httr::modify_url(base_url, path = path)

  # request API for user collection
  req <- discogs_get(url = url, ua,
                     httr::add_headers(Authorization=glue::glue("Discogs token={access_token}")
                                       )
                     )

  # break if user doesnt exist
  check_status(req)

  # break if object isnt json
  check_type(req)


  # EXTRACT DATA --------------------------------------

  # extract request content
  data <- jsonlite::fromJSON(httr::content(req, "text", encoding = "UTF-8"),
                             simplifyVector = FALSE)

  # how many collection pages?
  pages <- data$pagination$pages

  # iterate through pages of collection
  collection <- purrr::map(seq_len(pages), function(x){

    # request collection page
    req <- discogs_get(url = paste0(url, "&page=", x), ua,
                       httr::add_headers(Authorization=glue::glue("Discogs token={access_token}")
                                         )
                       )

    # break if user doesnt exist
    check_status(req)

    # break if object isnt json
    check_type(req)

    # extract request content
    if (simplify_df) {

      data <- jsonlite::fromJSON(httr::content(req, "text", encoding = "UTF-8"),
                                 simplifyDataFrame = TRUE)

    } else {

      data <- jsonlite::fromJSON(httr::content(req, "text", encoding = "UTF-8"),
                                 simplifyVector = FALSE)
    }

    # extract releases
    data <- data$releases

  })

  # combine pages
  if (simplify_df) {

    collection <- purrr::map_dfr(collection, jsonlite::flatten)

  } else {

    collection <- unlist(collection, recursive = F)

    }


  # create s3 object
  structure(
    list(
      content = collection,
      path = path,
      response = req
    ),
    class = "discogs_collection"
  )
}
