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
#' @param dates multiple dates to return data for
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
  obj$country <- country
  obj$date <- date
  class(obj) <- c("covidtracker_actions", "list")
  if(!test_covidtracker_actions(obj)){
    cat("\n")
    warning("Incorrect object for ", country, " ", date, ". NULL returned.", call. = FALSE)
    return(NULL)
  }
  obj
}

assert_covidtracker_actions <- function(x){
  checkmate::assert_class(x, "covidtracker_actions")
  checkmate::assert_names(names(x), identical.to = c("policyActions", "stringencyData", "country", "date"))
  checkmate::assert_names(names(x$stringencyData), identical.to = c("date_value", "country_code", "confirmed", "deaths", "stringency_actual", "stringency"))
}

test_covidtracker_actions <- function(x){
  !inherits(try(assert_covidtracker_actions(x), silent = TRUE), "try-error")
}

#' @rdname covidtracker_date_range
#' @export
covidtracker_actions_range <- function(countries, from, to, verbose = TRUE){
  checkmate::assert_character(countries, pattern = "[A-Za-z]{3}")
  from <- as.Date(from); to <- as.Date(to)
  checkmate::assert_date(from)
  checkmate::assert_date(to)
  checkmate::assert_true(from <= to)
  date_seq <- seq(from, to, by = 1)
  covidtracker_actions_dates(countries, date_seq, verbose)
}

#' @rdname covidtracker_date_range
#' @export
covidtracker_actions_dates <- function(countries, dates, verbose = TRUE){
  checkmate::assert_character(countries, pattern = "[A-Za-z]{3}")
  dates <- as.Date(dates)
  checkmate::assert_date(dates, unique = TRUE)
  gr <- expand.grid(country = countries, date = dates, stringsAsFactors = FALSE)

  if(verbose) pb <- txtProgressBar(min = 1, max = nrow(gr), style = 3)
  cta_list <- list()
  for (i in 1:nrow(gr)){
    if(verbose) setTxtProgressBar(pb, i)
    cta_list[[i]] <- covidtracker_actions(country = gr$country[i], date = gr$date[i])
  }
  cta_list
}

#' Extract data from covidtracker objects
#'
#' @param x a [covidtracker_actions] object to extract data from
#' @param ... further arguments to methods.
#'
#' @details
#' [data_frame_actions] extract policy actions together with date and country code
#' [data_frame_stringency] extract cases, deaths and stringency index together with date and country code
#'
#' @export
data_frame_actions <- function(x, ...){
  UseMethod("data_frame_actions")
}

#' @export
data_frame_actions.list <- function(x, ...){
  x <- lapply(x, data_frame_actions)
  do.call(rbind, x)
}

#' @export
data_frame_actions.covidtracker_actions <- function(x, ...){
  # Replace NULL with ""
  xpa <- lapply(x[["policyActions"]], function(x) lapply(x, function(x) if(is.null(x)) return("") else return(x)))
  xpa <- do.call(rbind, lapply(xpa, as.data.frame, stringsAsFactors = FALSE))
  xpa$policyvalue <- as.numeric(xpa$policyvalue)
  xpa$isgeneral <- suppressWarnings(as.logical(xpa$isgeneral))
  xpa$date <- x$stringencyData$date_value
  xpa$country_code <- x$stringencyData$country_code
  nc <- ncol(xpa)
  xpa <- xpa[, c((nc - 1):nc, 1:(nc-2))]
  xpa
}

#' @rdname data_frame_actions
#' @export
data_frame_stringency <- function(x){
  checkmate::assert_class(x, "covidtracker_actions")
  xpa <- data.frame(date = x$stringencyData$date_value)
  xpa$country_code <- x$stringencyData$country_code
  xpa$confirmed <- x$stringencyData$confirmed
  xpa$deaths <- x$stringencyData$deaths
  xpa$stringency_actual <- x$stringencyData$stringency_actual
  xpa$stringency <- x$stringencyData$stringency
  xpa
}
