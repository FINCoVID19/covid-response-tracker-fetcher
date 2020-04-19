#' Access COVID Tracker API
#'
#' @description
#' Access data on government reactions to COVID-19 pandemic
#' from the COVID Tracker API.
#'
#' @details
#' [covidtracker_date_range()] return stringency index for all countries for the given date range.
#' [covidtracker_actions()] return the governmental actions for the given date and country.
#' [covidtracker_actions_range()] wrapper functions to return government actions for multiple countries and date range.
#'
#' @param from the first date to return data for
#' @param to the last date to return data for
#' @param date the date to return data for
#' @param country a ISO 3166-1 alpha-3 country code
#' @param countries a vector of ISO 3166-1 alpha-3 country codes
#' @param verbose show progress of data download
#'
#' @references
#' \url{https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker}
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @examples
#' cta <- covidtracker_actions("FIN", "2020-03-15")
#'
#'
#' @export
covidtracker_date_range <- function(from, to){
  from <- as.Date(from); to <- as.Date(to)
  checkmate::assert_date(from)
  checkmate::assert_date(to)
  checkmate::assert_true(from <= to)
  r <- httr::GET(paste0("https://covidtrackerapi.bsg.ox.ac.uk/api/stringency/date-range/", from, "/", to))
  obj <- httr::content(r)
  class(obj) <- c("covidtracker_date_range", "list")
  obj
}

#' @rdname covidtracker_date_range
#' @export
covidtracker_actions <- function(country, date){
  date <- as.Date(date)
  checkmate::assert_date(date)
  checkmate::assert_string(country, pattern = "[A-Za-z]{3}")
  r <- httr::GET(paste0("https://covidtrackerapi.bsg.ox.ac.uk/api/stringency/actions/", country, "/", date))
  obj <- httr::content(r)
  class(obj) <- c("covidtracker_actions", "list")
  obj
}

#' @rdname covidtracker_date_range
#' @export
covidtracker_actions_range <- function(countries = c("FIN", "SWE"), from = "2020-03-01", to = "2020-03-05", verbose = TRUE){
  checkmate::assert_character(countries, pattern = "[A-Za-z]{3}")
  from <- as.Date(from); to <- as.Date(to)
  checkmate::assert_date(from)
  checkmate::assert_date(to)
  checkmate::assert_true(from <= to)
  date_seq <- seq(from, to, by = 1)
  gr <- expand.grid(country = countries, date = date_seq, stringsAsFactors = FALSE)

  if(verbose) pb <- txtProgressBar(min = 1, max = nrow(gr), style = 3)
  cta_list <- list()
  for (i in 1:nrow(gr)){
    if(verbose) setTxtProgressBar(pb, i)
    cta_list[[i]] <- covidtracker_actions(country = gr$country[i], date = gr$date[i])
  }
  cta_list
}

#' @export
as.data.frame.covidtracker_actions <- function(x, ...){
  # Replace NULL with ""
  xpa <- lapply(x[["policyActions"]], function(x) lapply(x, function(x) if(is.null(x)) return("") else return(x)))
  xpa <- do.call(rbind, lapply(xpa, as.data.frame, stringsAsFactors = FALSE))
  xpa$date <- x$stringencyData$date_value
  xpa$country_code <- x$stringencyData$country_code
  xpa$confirmed <- x$stringencyData$confirmed
  xpa$deaths <- x$stringencyData$deaths
  xpa$stringency_actual <- x$stringencyData$stringency_actual
  xpa$stringency <- x$stringencyData$stringency
  nc <- ncol(xpa)
  xpa <- xpa[, c((nc - 5):nc, 1:(nc-6))]
  xpa
}