#' Get metadata for a Discogs Label
#'
#' Return metadata for a Label (a label, company, recording studio, location,
#' or other entity involved with Artists and Releases) listed on Discogs.
#'
#' @param label_id The ID of the Label.
#'
#' @param access_token Discogs personal access token, defaults to \code{discogs_api_token}.
#'
#' @return a \code{discogs_database} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @export
#' @examples \dontrun{
#' discogs_label(label_id = 314)
#' }
discogs_label <- function(label_id, access_token=discogs_api_token()) {

  # check for internet
  check_internet()

  # create path
  path <- glue("labels/{label_id}")

  # base API users URL
  url <- modify_url(base_url, path = path)

  # request API for label
  req <- discogs_get(
    url = url, ua,
    add_headers(Authorization = glue("Discogs token={access_token}")
                )
    )

  # break if artist doesnt exist
  check_status(req)

  # break if object isnt json
  check_type(req)

  # extract request content
  data <- fromJSON(
    content(req, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
    )

  # create s3 object
  structure(
    list(
      content = data,
      path = path,
      response = req
    ),
    class = "discogs_database"
  )
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
#' @return a \code{discogs_database} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @export
#' @examples \dontrun{
#' discogs_label_releases(label_id = 314)
#' }
discogs_label_releases <- function(label_id, access_token = discogs_api_token()) {

  check_internet()

  path <- glue("labels/{label_id}/releases?")

  url <- modify_url(base_url, path = path)

  # request API for label releases
  req <- discogs_get(
    url = url, ua,
    add_headers(Authorization = glue("Discogs token={access_token}")
                )
    )

  check_status(req)
  check_type(req)

  # extract request content
  data <- fromJSON(
    content(req, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
    )

  # how many pages?
  pages <- data$pagination$pages

  # iterate through pages
  label_discogs <- lapply(seq_len(pages), function(x){

    # request label page
    req <- discogs_get(
      url = glue("{url}page={x}"), ua,
      add_headers(Authorization = glue("Discogs token={access_token}")
                  )
      )

    stop_for_status(req)
    check_type(req)

    # extract request content
    data <- fromJSON(
      content(req, "text", encoding = "UTF-8"),
      simplifyVector = TRUE, flatten = TRUE
      )

    rbind.data.frame(data$releases)

  })

  label_discogs <- do.call("rbind", label_discogs)

  # add artist id
  label_discogs$label_id <- label_id

  # create s3 object
  structure(
    list(
      content = label_discogs,
      path = path,
      response = req
    ),
    class = "discogs_database"
  )
}
