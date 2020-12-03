# **Building R Packages**
# Peer-graded Assignment: Documenting Code

#' @title fars_read
#'
#' @description The fars_read function reads a Fatality Analysis Reporting
#' System (FARS) data set that is in csv format. The file to read can be
#' customized using the \code{filename} argument.
#'
#' @param filename A file path to the data file.
#'
#' @return The function returns the data from the csv file as a "data.frame" and
#' if not saved to a variable name, it prints information about the data set to
#' the console. The function contains an "if" condition that prints an error if
#' the data set does not exist in the given location.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples \dontrun{
#' fars_read("2020_was_no_accident.csv.bz2")
#' fars.df <- fars_read("2020_was_no_accident.csv.bz2")
#' }
#'
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data) #deprecated, please use tibble::as_tibble() instead.
}


#' @title make_filename
#'
#' @description The make_filename function creates a standard file name in the
#' form of the string 'accident_', the year, which can be altered via the
#' \code{year} argument, and the file format (.csv.bz2).
#'
#' @param year A number or a vector of numbers describing the
#' year(s).
#'
#' @return A standard file name which looks like 'accident_year.csv.bz2'. This
#' name is printed when not saved to a variable name.
#'
#' @examples \dontrun{
#' make_filename(2020)
#' make_filename(c(2018, 2019))
#' new_filename <- make_filename(2017)
#' }
#'
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' @title fars_read_years
#'
#' @description The fars_read_years function gives an overview of how many
#' accidents have occurred in each month of a given year. You can customize the
#' year(s) of which you want an overview with the \code{years} argument, which
#' accepts a single year number or a vector of year numbers. The function reads
#' the specified FARS data sets and mutates a new variable ("year") for the
#' certain year, and selects the "MONTH" attribute from the data set, then gives
#' a list of data tables of these two attributes as a result. An error will
#' occur if the given year(s) is/are invalid.
#'
#' @param years A number or a vector of numbers describing the year(s) to
#' analyze.
#'
#' @return A list containing the data tables with two variables ("MONTH",
#' "year"). Returns NULL in case of an error.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples \dontrun{
#' fars_read_years(2012)
#' fars_read_years(c(2018, 2019))
#' fars.nineties <- fars_read_years(1990:2000)
#' }
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


#' @title fars_summarize_years
#'
#' @description The fars_summarize_years function reads yearly data of FARS
#' data sets and summarizes the number of accidents by month for each year
#' specified. You can customize the year(s) of which you want a summary with the
#' \code{years} argument, which accepts a single year number or a vector of year
#' numbers. This function passes the specified years to the fars_read_years
#' function, takes the outputted list of data tables and groups the
#' observations by year and month, and summarizes the number of accidents into a
#' dataframe.
#'
#' @param years A number or a vector of numbers describing the year(s) to
#' analyze.
#'
#'
#' @return A dataframe describing the number of accidents for the given year(s).
#'
#' @importFrom  dplyr bind_rows
#' @importFrom  dplyr group_by
#' @importFrom  dplyr summarize
#' @importFrom  tidyr spread
#'
#' @examples \dontrun{
#' fars_summarize_years(2017)
#' fars_summarize_years(c(2004, 2006, 2008))
#' fars.summary <- fars_summarize_years(2019)
#' }
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' @title fars_map_state
#'
#' @description The fars_map_state function reads yearly data of FARS datasets,
#' filters the data to the given state number, and plots the recorded accidents
#' to their geographic locations based on latitude and longitude data from the
#' data file. The state number and the years of which you want to plot can be
#' customized using the \code{state.num} and \code{year} arguments. The function
#' uses the make_filename and the fars_read functions to read the data sets for
#' the specified year(s), then filters the data for the given state number, and
#' plots the accidents to a map of the specific state based on the locations of
#' accidents. An error will occur if the given state number is invalid.
#'
#' @param state.num A number corresponding to a state.
#' @param year A number describing the year of interest.
#'
#' @return A map plot of a given state visualizing the accidents based on their
#' location.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples \dontrun{
#' fars_map_state(1, 2019)
#' fars_map_state(2, 2018)
#' }
#'
#' @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
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
