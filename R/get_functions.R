
get_clients <- function(){
  my_response <- httr::VERB(
    verb = "GET",
    url = paste0("https://api.clockify.me/api/workspaces/",
                 keyring::key_get("clockify_wsid"),
                 "/clients"),
    httr::add_headers(`X-Api-Key` = keyring::key_get("clockify_pw")),
    encode = "json"
  )
  #
  my_content <- httr::content(client_response) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(name)
  return(my_content)
}

# get the times
get_times <- function(check_first = FALSE){
  page <- 0
  content_list <- list()

  if (!check_first) {
    while (page > -1) {
      my_response <- httr::VERB(
        verb = "GET",
        url = paste0(
          "https://api.clockify.me/api/workspaces/",
          keyring::key_get("clockify_wsid"),
          "/timeEntries/?page=",
          page
        ),
        httr::add_headers(`X-Api-Key` = keyring::key_get("clockify_pw")),
        encode = "json"
      )
      tmp_content <- httr::content(my_response)
      if (length(tmp_content) > 0) {
        content_list[[page + 1]] <- scrape_times(tmp_content)
        page <- page + 1
      } else {
        page <- -100
      }
    }
  }
  # combine the output and return
  content_df <- dplyr::bind_rows(content_list)
  content_df$start <- lubridate::ymd_hms(content_df$start)
  content_df$end <- lubridate::ymd_hms(content_df$end)
  return(content_df)

}

# this goes through the nested list and returns
scrape_times <- function(one_list = NULL){
  do.call(rbind,
          lapply(unname(one_list),
                 function(y){
                   data.frame(
                     description = ifelse(!is.null(y[[which(names(y) == "description")]]),
                                          y[[which(names(y) == "description")]],
                                          NA),
                     tags = ifelse(!is.null(y[[which(names(y) == "tags")]]),
                                   sapply(y[[which(names(y) == "tags")]],
                                          function(x){x[[1]]}),
                                   NA),
                     project = y[[which(names(y) == "projectId")]],
                     time_id = y[[which(names(y)=="id")]],
                     start = y[[which(names(y) =="timeInterval")]][[1]],
                     end = y[[which(names(y) =="timeInterval")]][[2]],
                     stringsAsFactors = FALSE
                   )})) %>% tibble::as.tibble()
}


