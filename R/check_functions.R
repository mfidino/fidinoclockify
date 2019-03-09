check_clients <- function(clients = NULL){
  # check if the file exists, create if not.
  if(!file.exists("./Data/fidino_clients.csv")){
    write.csv(clients,"./Data/fidino_clients.csv", row.names = FALSE)
  }
  # if it does exist, check to see if any new clients need to be added.
  if(file.exists("./Data/fidino_clients.csv")){
    clients_csv <-read.csv("./Data/fidino_clients.csv",
                          stringsAsFactors = FALSE) %>%
      tibble::as_tibble()
    # check to see if they are not identical
    #  if not, add new records and append to file.
    if(!identical(clients_csv, clients)){
      clients_to_add <- dplyr::anti_join(clients, clients_csv,"id")
      cat(crayon::cyan("Adding new records to clients data."))
        write.table(clients_to_add,
                    "./Data/fidino_clients.csv",
                    append = TRUE,
                    row.names = FALSE,
                    col.names = FALSE,
                    sep = ",")
        # check the csv back out after adding new records
        clients_csv <- read.csv("./Data/fidino_clients.csv",
                                stringsAsFactors = FALSE) %>%
          tibble::as_tibble()
    }

    # check to see if a name needs to be updated. This
    #  will occur if a client name get's updated on
    #  clockify.
    tmp <- dplyr::left_join(clients,
                            clients_csv,
                            by = "id")
    # will return TRUE if names are not identical
      if(any(tmp$name.x != tmp$name.y)) {
        # which one needs to be updated from clients to csv
        to_update <- which(tmp$name.x != tmp$name.y)
        cat(crayon::yellow("Updating names in clients data."))
        # update
        clients_csv$name[to_update] <- clients$name[to_update]
        #save
        write.csv(clients_csv, "./Data/fidino_clients.csv",
                  row.names = FALSE)
      }
    }
}

check_times <- function(times = NULL){
  if(!file.exists("./Data/fidino_times.csv")){
    write.csv(times,"./Data/fidino_times.csv", row.names = FALSE)
  }
  # if it does exist, check to see if any new times need to be added.
  if(file.exists("./Data/fidino_times.csv")){
    times_csv <-read.csv("./Data/fidino_times.csv",
                           stringsAsFactors = FALSE) %>%
      tibble::as_tibble()
    # check to see if they are not identical
    #  if not, add new records and append to file.
    if(!identical(times_csv, times)){
      times_to_add <- dplyr::anti_join(times ,times_csv,"id")
      cat(crayon::cyan("Adding new records to times data."))
      write.table(times_to_add,
                  "./Data/fidino_times.csv",
                  append = TRUE,
                  row.names = FALSE,
                  col.names = FALSE,
                  sep = ",")
    }
    }
}

check_tibble <- function(x){
  if(tibble::is_tibble(x) & nrow(x) > 0) {
    cat(crayon::green( cli::symbol$tick), "collected" )
  } else {
    cat(crayon::red( cli::symbol$cross), "not collected")
  }
}
