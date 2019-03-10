
report_summary <- function(x = NULL, next_meeting = NULL){
  # apply
  if(is.character(next_meeting)){
    mdate <- lubridate::ymd(next_meeting)
    if(is.na(mdate)){
      stop("\n\nnext_meeting must be the format 'YYYY-m-d'.")
    }
  }
  if(!file.exists("./Data/meeting_days.csv")){
    err <- paste0("\n", crayon::red('do_summary()'),
                  " requires a 'meeting_days.csv' in the Data subfolder.\n",
                  "If meetings are always on a given day,\nuse ",
                  crayon::red('meeting_days()'), " to generate.")
    stop(err)
  } else {
    days <- read.csv("./Data/meeting_days.csv") %>%
      dplyr::mutate_all(lubridate::ymd) %>%
      tibble::as_tibble(.)
  }
  if(mdate %in% days$start){
    trange <- days[(which(days$start == mdate) - 1),]
  } else {
    stop("\n\n next_meeting not in 'meeting_days.csv'")
  }
  cat(cli::rule(center = " * SUMMARIZING DATA * ", col = "purple"),"\n")
  first_pass <- x %>%
    dplyr::filter(dplyr::between(as.Date(.$start), trange$start, trange$end)) %>%
    dplyr::mutate(dur = as.numeric(.$end - .$start)) %>%
    dplyr::mutate(prop_time = dur / sum(dur))
  cat(crayon::cyan( cli::symbol$bullet," Time spent on each project:   "))
  by_project <- first_pass %>%
    dplyr::group_by(project_name) %>%
    dplyr::summarise(prop_time = sum(prop_time)) %>%
    dplyr::arrange(dplyr::desc(prop_time))
  check_tibble(by_project)
  cat(crayon::cyan( cli::symbol$bullet," Time spent on each client:    "))
  by_client <- first_pass %>%
    dplyr::group_by(client_name) %>%
    dplyr::summarise(prop_time = sum(prop_time)) %>%
    dplyr::arrange(dplyr::desc(prop_time))
  check_tibble(by_client)
  cat(crayon::cyan( cli::symbol$bullet," Weeks since research visited: "))
  weeks_since <- x %>% dplyr::group_by(project_name) %>%
    dplyr::filter( is_research) %>%
    dplyr::filter( start == max(start)) %>%
    dplyr::select( start, project_name) %>%
    dplyr::summarise(last_touch = difftime(Sys.Date(), as.Date(start),
                                           units = "weeks") %>%
                       ceiling) %>%
    dplyr::arrange(dplyr::desc(last_touch)) %>%
    dplyr::left_join(., x, by = "project_name") %>%
    dplyr::select(project_name, client_name, last_touch) %>%
    dplyr::distinct()
  check_tibble(weeks_since)

  # do
  return(list(project = by_project,
              client = by_client,
              weeks_since = weeks_since))

}
