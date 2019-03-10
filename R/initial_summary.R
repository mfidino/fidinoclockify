source("./R/get_functions.R")
source("./R/check_functions.R")
source("./R/scrape_functions.R")
source("./R/utility_functions.R")
source("./R/report_functions.R")

require("magrittr")

data <- get_all() %>% report_summary(., "2019-3-18")




