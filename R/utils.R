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

base_url <- "https://api.discogs.com/"
