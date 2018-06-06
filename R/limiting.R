#' Check Discogs API user rate limits
#'
#' This function prints a users Discogs API rate limit information. All calls to the API
#' deduct one unit from rate limits. Discogs rate limiting tracks your requests using a moving
#' average over a 60 second window. If no requests are made in 60 seconds, your window will reset.
#' For more information, check the \href{https://www.discogs.com/developers/#page:home,header:home-rate-limiting}{rate limiting API documentation}
#'
#' @param access_token Discogs personal access token, defaults to \code{discogs_api_token}.
#'
#' @return None (invisible NULL)
#'
#' @export
#' @examples \dontrun{
#' discogs_rate_limit()
#' }
discogs_rate_limit <- function(access_token=discogs_api_token()) {

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