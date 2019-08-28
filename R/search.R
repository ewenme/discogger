discogs_search <- function(params, n = 100, access_token=discogs_api_token()) {

  # check for internet
  check_internet()

  # ensure params passed as a list
  stopifnot(is.list(params))

  # turn param list into a string
  param_string <- glue("{names(params)}={params}")

  # collapse
  param_string <- paste(param_string, collapse = "&")

  # construct path
  path <- paste0("/database/search?", param_string)

  # base API users URL
  url <- modify_url(base_url, path = path)

  # request API for user collection
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

  data

  # how many pages returned?
  # pages <- data$pagination$pages

  # how many items


}
