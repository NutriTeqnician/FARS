install.packages("roxygen2")
library(roxygen2)

#' Read csv files from FARS data folder
#' This function checks if csv files are present in the FARS data folder, reads it and returns a data frame.
#'
#' @param filename the csv file to be read
#'
#' @return A data frame containing the data in the csv file in the FARS data folder.
#'
#' @examples
#' fars_data <- fars_read("accident_2013.csv.bz2")
#'
#' @export
fars_read <- function(filename) {
  if (!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Generate a filename for a given year for a csv file
#' This function generates a year specific filename to be retrieved from FARS data folder
#' The filename format is "accident_{year}.csv.bz2".
#'
#' @param The year for which the filename is generated.
#'
#' @return A character string representing the filename.
#'
#' @examples
#' file_name <- make_filename(2022)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read FARS data for multiple years
#'
#' @param years A vector of years
#'
#' @return A list of `tibble`s, one for each year
#'
#' @examples
#' fars_read_years(2013:2015)
#'
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>% 
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}




#' Calculate descriptives of data in csv files in FARS data folder
#'
#' This function reads and summarizes data in csv files in the FARS data folder for a list of years, providing accident counts by year and month.
#'
#' @param years A vector of years for which data should be summarized.
#'
#' @return A data frame with year and month as columns, along with accident counts for each year.
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}




#' Create a map of FARS accidents for a given state and year
#'
#' @param state.num The state number
#' This function plots FARS accident data on a map for a given state and year.
#' It checks for the validity of the state number and handles missing or invalid longitude and latitude values.
#'
#' @param year The year of the FARS data
#' @return A map plot with accident locations.
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if (!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if (nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
