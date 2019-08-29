#' Search Discogs database
#'
#' Issue a search query to the Discogs database using any number of
#' (optional) parameters.
#'
#' @param params named list of parameters
#' (see \href{https://www.discogs.com/developers/#page:database,header:database-search}{SEARCH API docs}
#' for available parameters)
#' @param n_results (optional) set limit on no. of results (numeric; NULL by default)
#' @inheritParams discogs_artist
#'
#' @examples \dontrun{
#' discogs_search(params = list(release_title = "Purple Rain",
#' artist = "Prince"), n_results = 10)
#' }
#'
#' @keywords internal

discogs_search <- function(params, n_results = NULL,
                           access_token = discogs_api_token()) {

  check_internet()
  stopifnot(is.list(params))

  # turn param list into a string
  param_string <- glue("{names(params)}={params}")

  # collapse
  param_string <- paste(param_string, collapse = "&")

  path <- glue("/database/search?{param_string}")

  url <- modify_url(base_url, path = path)

  # request API for user collection
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

  # limit pages returned if results limit set
  if (is.numeric(n_results)) {

    pages_limit <- ceiling(n_results / 50)

    if (pages_limit > pages) pages_limit <- pages

    pages <- pages_limit
  }

  search_discogs <- lapply(seq_len(pages), function(x){

    # request artist page
    req <- discogs_get(
      url = paste0(url, "&page=", x), ua,
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

    bind_rows(data$results)

  })

  search_discogs <- bind_rows(search_discogs)

  # limit result set
  if (is.numeric(n_results)) {
    search_discogs <- search_discogs[1:n_results, ]
  }

  # create s3 object
  structure(
    list(
      content = search_discogs,
      path = path,
      response = req
    ),
    class = "discogs_database"
  )

}
