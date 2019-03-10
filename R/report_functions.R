
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
              weeks_since = weeks_since,
              time_range = trange))
}

plot_projects <- function(x, filename = "project_plot.tiff"){
  if(!all(colnames(x) %in% c("project_name", "prop_time"))){
    err <- paste0("\nWrong table supplied to ",
                  crayon::red("plot_projects()","."))
    stop(err)
  }
  tiff(paste0("./images/",filename), height = 7, width = 7,
       units = "in", res = 400, compression = "lzw")
  par(mar =c(6,12,0.5,1))

  if(max(x$prop_time) < 0.5){
    xmax <- 0.5
  } else if(max(x$prop_time) > 0.5){
    xmax <- 0.8
  } else if(max(x$prop_time) < 0.75){
    xmax <- 1
  }
  suppressWarnings(plot(1~1, ylim = c(1, nrow(x)), xlim = c(0, xmax), bty = 'l',
                        xaxt="n", type="n", yaxt="n", xlab="",ylab=""))

  axis(side = 1, at = seq(0, xmax, 0.1),
       tck = -0.025,labels = NA)
  axis(side = 1, at = seq(0, xmax, 0.05),
       tck = -0.025/2,labels = NA)
  axis(side = 2, at = seq(1, nrow(x)),
       tck = -0.025, labels = NA)

  abline(h = 1:nrow(x), col = "gray90")
  abline(v=seq(0,xmax,0.1), col = "gray90")
  go_by <- ifelse(xmax == 1, 0.2, 0.1)
  mtext(text = seq(0, xmax, go_by), side = 1,
        at = seq(0, xmax, go_by), line = 0.6, cex = 1.2)

  palette <- colorRampPalette(colors=c("#bcbddc", "#4a1486"))
  cols <- palette(dplyr::n_distinct(x$project_name))

  mtext(text = x$project_name, side = 2, las = 1,
        at = rev(seq(1, nrow(x))), line = .9, cex = 1.2)
  mtext(text = "Proportion of time spent\non projects", 1,
        at = xmax / 2, line = 4.20, cex = 1.75)

  points(y = rev(seq(1, nrow(x))), x = x$prop_time, pch = 21,
         cex = 2, bg = rev(cols))
  dev.off()

}
