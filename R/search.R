discogs_search <- function(q, n = 100, access_token=discogs_api_token()) {

  # check for internet
  check_internet()

  # API REQUEST ---------------------------------------

  # create path
  path <- paste0("/database/search?q=", q)

  # base API users URL
  url <- httr::modify_url(base_url, path = path)

  # request API for user collection
  req <- httr::GET(url = url, ua,
                   httr::add_headers(Authorization=paste0("Discogs token=", access_token))
  )

  # break if artist doesnt exist
  check_status(req)

  # break if object isnt json
  check_type(req)


  # EXTRACT DATA --------------------------------------

  # extract request content
  data <- jsonlite::fromJSON(httr::content(req, "text", encoding = "UTF-8"),
                             simplifyVector = FALSE)

  # how many pages returned?
  # pages <- data$pagination$pages

  # how many items


}
