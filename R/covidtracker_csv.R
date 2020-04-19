#' Create and store API data as CSV files
#'
#' @param file csv-file to store or update
#' @inheritParams covidtracker_date_range
#'
#' @importFrom utils read.csv write.csv
#'
#' @details
#' [covidtracker_create_actions_csv()] create a csv file to store response information in
#' [covidtracker_create_update_csv()] update an existing csv file with new data
#'
#' @export
covidtracker_update_actions_csv <- function(file, from = NULL, to = Sys.Date(), verbose = TRUE){
  checkmate::assert_file_exists(file)
  from <- as.Date(from); to <- as.Date(to)
  checkmate::assert_date(from, null.ok = TRUE)
  checkmate::assert_date(to)

  x <- read.csv(file, stringsAsFactors = FALSE)
  checkmate::assert_names(colnames(x), must.include = c("date", "country_code", "policy_type_code"))
  colnames <- colnames(x)
  countries <- unique(x$country_code)
  dates <- as.Date(unique(x$date))
  x[is.na(x)] <- ""
  if(is.null(from)) from <- dates[which.min(dates)]

  update_dates <- seq(from, to, by = 1)
  update_dates <- update_dates[!update_dates %in% dates]

  ctad <- covidtracker_actions_dates(countries, update_dates, verbose = TRUE)
  ctad <- data_frame_actions(ctad)
  ctad <- ctad[,colnames]

  x <- rbind(x, ctad)
  x[order(x$date, x$country_code, x$policy_type_code),]
}

#' @rdname covidtracker_update_actions_csv
#' @export
covidtracker_create_actions_csv <- function(file, countries, from, to, verbose = TRUE){
  checkmate::assert_path_for_output(file)
  checkmate::assert_character(countries, pattern = "[A-Za-z]{3}")
  from <- as.Date(from); to <- as.Date(to)
  checkmate::assert_date(from)
  checkmate::assert_date(to)
  checkmate::assert_flag(verbose)

  cta <- covidtracker_actions_range(countries, from, to, verbose)
  cta <- data_frame_actions(cta)
  cta <- cta[order(cta$date, cta$country_code, cta$policy_type_code),]
  write.csv(cta, file = file, row.names = FALSE)

}
