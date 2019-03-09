
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

