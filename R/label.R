#' Get metadata for a Discogs Label
#'
#' Return metadata for a Label (a label, company, recording studio, location,
#' or other entity involved with Artists and Releases) listed on Discogs.
#'
#' @param label_id The ID of the Label.
#'
#' @param access_token Discogs personal access token, defaults to \code{discogs_api_token}.
#'
#' @return a \code{discogs_label} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @export
#' @examples \dontrun{
#' discogs_label(label_id = 314)
#' }
discogs_label <- function(label_id, access_token=discogs_api_token()) {

  # check for internet
  check_internet()

  # API REQUEST ---------------------------------------

  # create path
  path <- paste0("labels/", label_id)

  # base API users URL
  url <- httr::modify_url(base_url, path = path)

  # request API for user collection
  req <- httr::GET(url = url)

  # break if artist doesnt exist
  check_status(req)

  # break if object isnt json
  check_type(req)


  # EXTRACT DATA ---------------------------------------

  # extract request content
  # data <- httr::content(req)
  data <- jsonlite::fromJSON(httr::content(req, "text", encoding = "UTF-8"),
                             simplifyVector = FALSE)

  # create s3 object
  structure(
    list(
      content = data,
      path = path,
      response = req
    ),
    class = "discogs_label"
  )
}

print.discogs_label <- function(x, ...) {

  cat("<Discogs ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)

}

#' Get metadata for a Discogs Label's Releases
#'
#' Return tidy metadata for a Label's (a label, company, recording studio, location,
#' or other entity involved with Artists and Releases) Releases listed on Discogs.
#'
#' @param label_id The ID of the Label.
#'
#' @param access_token Discogs personal access token, defaults to \code{discogs_api_token}.
#'
#' @return a \code{discogs_label_releases} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @export
#' @examples \dontrun{
#' discogs_label_releases(label_id = 314)
#' }
discogs_label_releases <- function(label_id, access_token=discogs_api_token()) {

  # check for internet
  check_internet()

  # API REQUEST ----------------------------------------------

  # create path
  path <- paste0("labels/", label_id, "/releases?")

  # base API users URL
  url <- httr::modify_url(base_url, path = path)

  # request API for user collection
  req <- httr::GET(url = url)

  # break if release doesnt exist
  check_status(req)

  # break if object isnt json
  check_type(req)


  # EXTRACT DATA --------------------------------------

  # extract request content
  data <- jsonlite::fromJSON(httr::content(req, "text", encoding = "UTF-8"),
                             simplifyVector = FALSE)

  # how many pages?
  pages <- data$pagination$pages


  # ITERATION -----------------------------------

  # iterate through pages
  label_discogs <- purrr::map_dfr(seq_len(pages), function(x){

    # request label page
    req <- httr::GET(url = paste0(url, "page=", 1))

    # break if artist doesnt exist
    httr::stop_for_status(req)

    # break if object isnt json
    check_type(req)

    # extract request content
    data <- jsonlite::fromJSON(httr::content(req, "text", encoding = "UTF-8"),
                               simplifyVector = FALSE)

    # bind releases
    release_info <- dplyr::bind_rows(data$releases)

  })

  # add artist id
  label_discogs$label_id <- label_id

  # create s3 object
  structure(
    list(
      content = label_discogs,
      path = path,
      response = req
    ),
    class = "discogs_label_releases"
  )
}

print.discogs_label_releases <- function(x, ...) {

  cat("<Discogs ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)

}
