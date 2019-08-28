# global parameters
base_url <- "https://api.discogs.com/"
ua <- user_agent("http://github.com/ewenme/discogger")

check_internet <- function(){
  stop_if_not(.x = has_internet(), msg = "Please check your internet connection")
}

check_status <- function(res){
  stop_if_not(.x = status_code(res),
              .p = ~ .x == 200,
              msg = "The API returned an error")
}

check_type <- function(res){
  stop_if_not(.x = http_type(res),
              .p = ~ .x == "application/json",
              msg = "The API did not return json")
}

discogs_get <- limit_rate(
  GET, rate(n = 1, period = 1)
)
