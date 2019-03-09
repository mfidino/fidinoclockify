source("./R/get_functions.R")
source("./R/check_functions.R")
source("./R/scrape_functions.R")

require("magrittr")

data <- get_all()

# starting with first monday of a given year
the_year <- lubridate::year(Sys.Date())
days <- tibble::tibble(start = seq(lubridate::ymd("2018-12-24"),
                     lubridate::ymd(paste0(the_year,"-12-31")), by = "1 day")) %>%
  dplyr::filter( weekdays(.$start) == "Monday" ) %>%
  dplyr::filter(dplyr::row_number() %% 2 == 1) %>%
  dplyr::mutate(end = lubridate::ymd(dplyr::lead(.$start)) - 3) %>%
  dplyr::filter(complete.cases(.))

mdate <- lubridate::ymd("2019-3-18")

trange <- days[(which(days$start == mdate) - 1),]

first_pass <- data %>%
  dplyr::filter(dplyr::between(as.Date(.$start), trange$start, trange$end)) %>%
  dplyr::mutate(dur = as.numeric(.$end - .$start)) %>%
  dplyr::mutate(prop_time = dur / sum(dur))

by_client <- first_pass %>%
  dplyr::group_by(client_name) %>%
  dplyr::summarise(prop_time = sum(prop_time)) %>%
  dplyr::arrange(dplyr::desc(prop_time))

by_project <- first_pass %>%
  dplyr::group_by(project_name) %>%
  dplyr::summarise(prop_time = sum(prop_time)) %>%
  dplyr::arrange(dplyr::desc(prop_time))

# the last time I have interacted with a specific project

days_since <- data %>% dplyr::group_by(project_name) %>%
  dplyr::filter( is_research) %>%
  dplyr::filter( start == max(start)) %>%
  dplyr::select( start, project_name) %>%
  dplyr::summarise(last_touch = Sys.Date() - as.Date(start)) %>%
  dplyr::arrange(last_touch) %>%
  dplyr::left_join(., data, by = "project_name") %>%
  dplyr::select(project_name, client_name, last_touch) %>%
  dplyr::distinct()


hm <- data %>% dplyr::group_by(week) %>%
  dplyr::mutate(prop_time = duration / sum(as.numeric(duration)))

query <- function(x)
