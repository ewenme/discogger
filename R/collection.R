get_discogs_collection <- function(user_name, access_token=discogs_token()) {

  # set base URL
  base_url <- "https://api.discogs.com//users/"

  # set tail of URL
  tail_url <- "/collection/folders/0/releases"

  # set user name
  user <- "sunshine-recorder"

  # construct user collection request URL
  req_url <- paste0(base_url, user, tail_url)

  # request API for user collection
  req <- httr::GET(url = req_url)

  # extract request content
  res <- httr::content(req)

  # get no. of pages
  pages <- res[['pagination']][['pages']]

  # allocate list of same length as pages
  collection <- vector("list", pages)

  #loop through collection pages and bind to dataframe
  for(i in 1:length(collection)) {

    # request given page of user collection
    data <- httr::GET(paste0(req_url, "?page=", i))

    # extract request content
    data <- httr::content(data)

    # extract track info from returned results
    release_info <- purrr::map_df(1:length(data$releases), function(x) {
      tmp <- data$releases[[x]]
      info <- data$releases[[x]]$basic_information
      labs <- unlist(data$releases[[x]]$basic_information$labels)
      artists <- unlist(data$releases[[x]]$basic_information$artists)
      formats <- unlist(data$releases[[x]]$basic_information$formats)
      format_descs <- unlist(data$releases[[x]]$basic_information$formats[[1]]$descriptions)

      list(
        instance_id = tmp$instance_id,
        date_added = tmp$date_added,
        release_id = tmp$id,
        rating = tmp$rating,
        release_title = info$title,
        label_name = labs[['name']],
        label_cat_no = labs[['catno']],
        label_id = labs[['id']],
        format_name = formats[['name']],
        format_descriptions = list(format_descs),
        # format_description_one = ifelse("descriptions1" %in% names(formats),
        #                                 formats[['descriptions1']],
        #                                 ifelse(formats[['descriptions']] %in% names(formats),
        #                                        formats[['descriptions']], NA)),
        # format_description_two = ifelse("descriptions2" %in% names(formats),
        #                                 formats[['descriptions2']], NA),
        # format_description_three = ifelse("descriptions3" %in% names(formats),
        #                                   formats[['descriptions3']], NA),
        artist_name = artists[['name']],
        artist_id = artists[['id']],
        cover_image <- info$cover_image,
        release_year <- info$year
      )
    })

    # update user on progress
    message("Retrieving page ", i)

    collection[[i]] <- release_info
  }

  foo <- dplyr::bind_rows(collection)

  bar <- unlist(data$releases[[1]]$basic_information$labels)


}
