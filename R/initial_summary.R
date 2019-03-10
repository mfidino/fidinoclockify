

data <- get_all() %>% report_summary(., "2019-3-18") %>% plot_all()

plot_projects(data$project)
plot_clients(data$client)
plot_weeks(data$weeks_since)


