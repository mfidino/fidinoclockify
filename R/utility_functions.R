
meeting_days <- function( first_meeting = "2018-12-24", by = "biweekly"){
  # Get the year from the computer
  the_year <- lubridate::year(Sys.Date())
  # convert first_meeting to a date if it is a character.
  if(is.character(first_meeting)){
    first_meeting <- lubridate::ymd(first_meeting)
  }
  # meeting frequency, used with modulus operator in filter
  mf <- switch(by,
    "weekly" = c(1, 0),
    "biweekly" = c(2, 1),
    "monthly" = c(4, 1)
  )
  tibble::tibble(start = seq(lubridate::ymd(first_meeting),
                             lubridate::ymd(paste0(the_year,"-12-31")),
                             by = "1 day")) %>%
    dplyr::filter( weekdays(.$start) == weekdays(first_meeting)) %>%
    dplyr::filter(dplyr::row_number() %% mf[1] == mf[2]) %>%
    dplyr::mutate(end = lubridate::ymd(dplyr::lead(.$start)) - 1) %>%
    dplyr::filter(complete.cases(.)) %>%
    return(.)
}
