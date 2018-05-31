#' Get metadata for a user's Discogs Collection
#'
#' Return metadata for releases in a user's Discogs collection.
#'
#' @param user_name The username of the Collection you are trying to request.
#'
#' @param folder_id The ID of the Collection folder (default value is 0,
#' the “All” folder).
#'
#' @param access_token Discogs personal access token, defaults to \code{discogs_api_token}.
#'
#' @return a \code{discogs_user_collection} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @export
#' @examples \dontrun{
#' discogs_user_collection(user_name = "rodneyfool")
#' }
discogs_user_collection <- function(user_name, folder_id=0, access_token=discogs_api_token()) {

  # check for internet
  check_internet()

  # API REQUEST ---------------------------------------

  # create path
  path <- paste0("users/", user_name, "/collection/folders/",
                 folder_id, "/releases?sort=added&sort_order=desc")

  # base API users URL
  url <- httr::modify_url(base_url, path = path)

  # request API for user collection
  req <- httr::GET(url = url)

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
    req <- httr::GET(url = paste0(url, "&page=", x))

    # break if user doesnt exist
    check_status(req)

    # break if object isnt json
    check_type(req)

    # extract request content
    data <- jsonlite::fromJSON(httr::content(req, "text", encoding = "UTF-8"),
                               simplifyVector = FALSE)

    # extract releases
    data <- data$releases

  })

  # combine lists
  collection <- unlist(collection, recursive = F)

  # create s3 object
  structure(
    list(
      content = collection,
      path = path,
      response = req
    ),
    class = "discogs_user_collection"
  )
}

print.discogs_user_collection <- function(x, ...) {

  cat("<Discogs ", x$path, ">\n", sep = "")
  x$content
  invisible(x)

}
