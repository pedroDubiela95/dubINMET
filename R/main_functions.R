#' @title It creates csv by year, and it creates the columns
#' Region, State and City. This function must be used when the data
#' are downloaded from https://portal.inmet.gov.br/dadoshistoricos.
#'
#' @description It aggregates and creates csv files by years.
#' It will save the results in folder results. This folder results
#' always will be created, if it already exists, it will be
#' deleted and after will be created again
#'
#' @param path \[\code{character}\]\cr
#' path where are the files
#'
#' @examples
#'  \dontrun{
#'  save_path <- files_by_year("/home/pedrodev/Documents/Base De Dados/INMET")
#' }
#'
#' @export
files_by_year <- function(path) {

  # Global variables
  . <- Regiao <- Estado <- Cidade <- NULL

  ano       <- list.files(path)
  dir       <- paste0(path, "/", ano)
  dt_res    <- data.table::data.table()
  save_path <- file.path(path, "../results")

  # if save directory already exists, so it will be deleted
  if(dir.exists(save_path)) {unlink(x = save_path, recursive = T, force = T)}

  # Crating new save directory
  dir.create(path = save_path)

  for (i in seq(length(dir))) {

    # Files by year
    f      <- list.files(dir[i])
    files  <- paste0(dir[i],"/",f)
    dt_res <- data.table::data.table()
    infos  <- stringr::str_split(f, pattern = "_")

    # Taking all files in each year
    for (j in seq(length(files))) {
      dt_res <- data.table::fread(files[j], skip = 9, data.table = T)
      dt_res <- data.table::fread(files[j], sep = ";", header = T, skip = 8) %>%
        .[, Regiao:=infos[[j]][2]] %>%
        .[, Estado:=infos[[j]][3]] %>%
        .[, Cidade:=infos[[j]][5]] %>%
        rbind(dt_res, .)
      gc()
    }

    # Creating csv file by year
    destination <- paste0(save_path,"/",ano[i], ".csv")
    data.table::fwrite(x = dt_res, file = destination, showProgress = T)
  }

  return(save_path)
}

#' @title Preprocessing data.table
#'
#' @description This function performs the follow
#' data.table transformations:
#' - Replace (,) with (.).
#' - Changes variable type to numeric type
#'
#' @param dt \[\code{data.table}\]\cr
#' data.table which will be transformed
#'
#' @examples
#'  \dontrun{
#'
#' }
#'
#' @export
preprocessing <- function(dt, colnames) {

  # Replace (,) with (.) and changes variable type to numeric type
  sapply(colnames, function(col){
    dt[[col]] <<- dt[[col]] %>%
      stringr::str_replace_all(string = ., pattern = ",", ".") %>%
      as.numeric(.)
  })

  return(dt)
}
