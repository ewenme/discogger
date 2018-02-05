get_discogs_collection <- function(user_name, sep_format_descs=TRUE, access_token=discogs_token()) {

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

  # loop through collection pages and bind to dataframe
  collection <- lapply(1:pages, function(x) {

    # request given page of user collection
    data <- httr::GET(paste0(req_url, "?page=", x))

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
        artist_name = artists[['name']],
        artist_id = artists[['id']],
        cover_image = info$cover_image,
        release_year = info$year
      )

    })

    })

  # bind dfs of collection
  collection_df <- do.call("rbind", collection)

  # separating format description list
  if (isTRUE(sep_format_descs)) {

    # find max format description length, fill NAs if less
    track_descs <- sapply(collection_df$format_descriptions, "length<-",
                          max(lengths(collection_df$format_descriptions)))

    # check no. track format description fields
    lengths <- lapply(track_descs, length)

    # ID max length
    longest <- max(unlist(lengths))

    # replace null items with NA
    track_descs <- lapply(track_descs, function(x) if(is.null(x)) NA else x)

    # turn track descs into df w/ incrementing col suffixes
    track_descs <- setNames(do.call(rbind.data.frame, track_descs),
                            paste("format_description", 1:longest, sep = "_"))

    # bind cols to collection
    collection_df <- cbind(collection_df, track_descs)

    # remove original format col
    collection_df <- subset(collection_df, select = -format_descriptions)

  }

  # do nothing if user wants to preserve current format descs
  else NULL

  return(collection_df)

}
