#' @importFrom attempt stop_if_not
#' @importFrom curl has_internet
check_internet <- function(){
  attempt::stop_if_not(.x = curl::has_internet(), msg = "Please check your internet connection")
}

#' @importFrom httr status_code
check_status <- function(res){
  attempt::stop_if_not(.x = httr::status_code(res),
              .p = ~ .x == 200,
              msg = "The API returned an error")
}

base_url <- "https://api.discogs.com/"

#' @importFrom httr http_type
check_type <- function(res){
  attempt::stop_if_not(.x = httr::http_type(res),
                       .p = ~ .x == "application/json",
                       msg = "The API did not return json")
}

discogs_rate_limit <- function() {

  # hit api
  req <- httr::GET(base_url, ua,
                   httr::add_headers(Authorization=paste0("Discogs token=", access_token)))

  # extract headers
  headers <- httr::headers(req)

  # print rate limit info

  cat(headers$`x-discogs-ratelimit-used`, " / ", headers$`x-discogs-ratelimit`,
      " (", headers$`x-discogs-ratelimit-remaining`, " requests remaining) as of ",
      headers$date, "\nN.B. Discogs rate limiting tracks your requests using a moving average over a 60 second window.", sep = "")
}

# global parameters
base_url <- "https://api.discogs.com/"
ua <- httr::user_agent("http://github.com/ewenme/discogger")
