
get_clients <- function(){
  client_response <- httr::VERB(
    verb = "GET",
    url = paste0("https://api.clockify.me/api/workspaces/",
                 keyring::key_get("clockify_wsid"),
                 "/clients"),
    httr::add_headers(`X-Api-Key` = keyring::key_get("clockify_pw")),
    encode = "json"
  )
  #
  client_content <- httr::content(client_response) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(name) %>%
    dplyr::select(., -dplyr::starts_with("workspace"))
  colnames(client_content) <- c("client_id", "client_name")
  return(client_content)
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

get_projects <- function(){
  project_response <- httr::VERB(
    verb = "GET",
    url = paste0("https://api.clockify.me/api/workspaces/",
                 keyring::key_get("clockify_wsid"),
                 "/projects/"),
    httr::add_headers(`X-Api-Key` = keyring::key_get("clockify_pw")),
    encode = "json"
  )
  #
  project_content <- httr::content(project_response) %>%
    scrape_projects
  return(project_content)
}

get_tags <- function(){
  tags_response <- httr::VERB(
    verb = "GET",
    url = paste0("https://api.clockify.me/api/workspaces/",
                 keyring::key_get("clockify_wsid"),
                 "/tags"),
    httr::add_headers(`X-Api-Key` = keyring::key_get("clockify_pw")),
    encode = "json"
  )
  #
  tags_content <- httr::content(tags_response) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(name) %>%
    dplyr::select(., -dplyr::starts_with("workspace"))
  colnames(tags_content) <- c("tags_id", "tags_name")
  return(tags_content)

}

get_all <- function(join = TRUE){
  cat(cli::rule(center = " * COLLECTING DATA * ", col = "purple"),"\n")
  # projects
  cat(crayon::cyan( cli::symbol$bullet," Projects: "))
  projects <- get_projects()
  check_tibble(projects)
  #clients
  cat(crayon::cyan( cli::symbol$bullet," Clients:  "))
  clients <- get_clients()
  check_tibble(clients)
  #tags
  cat(crayon::cyan( cli::symbol$bullet," Tags:     "))
  tags <- get_tags()
  check_tibble(tags)
  # times
  cat(crayon::cyan(cli::symbol$bullet, " Times:    "))
  times <- get_times()
  check_tibble(times)


  dplyr::left_join(times, projects, by = "project_id") %>%
    dplyr::left_join(., clients, by = "client_id") %>%
    dplyr::left_join(., tags, by = "tags_id") %>%
    dplyr::select(., -dplyr::ends_with("id")) %>%
    return

}
