#' @title It creates csv by year
#'
#' @description It aggregates and creates csv files by years.
#' It will save the results in folder results. This folder results
#' always will be created, if it already exists, it will be
#' deleted and after will be created again
#'
#' @param path \[\code{character}\]\cr
#' path where the files are
#'
#' @export
files_by_year <- function(path) {

  .         <- NULL
  ano       <- list.files(path)
  dir       <- paste0(path, "/", ano)
  dt_res    <- data.table::data.table()
  save_path <- file.path(path, "../results")

  #if save directory already exists, so it will be deleted
  if(dir.exists(save_path)) {unlink(x = save_path, recursive = T, force = T)}

  #Crating new save directory
  dir.create(path = save_path)

  for (i in seq(length(dir))) {

    #Files by year
    files  <- paste0(dir[i],"/",list.files(dir[i]))
    dt_res <- data.table::data.table()

    #Taking all files in each year
    for (file in files) {
      dt_res <- data.table::fread(file, sep = ";", header = T, skip = 8) %>% rbind(dt_res, .)
      gc()
    }

    #Creating csv file by year
    destination <- paste0(save_path,"/",ano[i], ".csv")
    data.table::fwrite(x = dt_res, file = destination, showProgress = T)
  }

  return(save_path)
}

