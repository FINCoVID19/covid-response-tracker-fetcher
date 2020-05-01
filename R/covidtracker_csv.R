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
#' [covidtracker_csv()] access and the published CSV-file directly
#' @export
covidtracker_update_actions_csv <- function(file, from = NULL, to = Sys.Date(), verbose = TRUE){
  checkmate::assert_file_exists(file)
  if(!is.null(from)) from <- as.Date(from);
  to <- as.Date(to)
  checkmate::assert_date(from, null.ok = TRUE)
  checkmate::assert_date(to)

  x <- read.csv(file, stringsAsFactors = FALSE)
  checkmate::assert_names(colnames(x), must.include = c("date", "country_code", "policy_type_code"))
  colnames <- colnames(x)
  countries <- unique(x$country_code)
  dates <- as.Date(unique(x$date))

  x$policyvalue <- as.numeric(x$policyvalue)
  x$isgeneral <- suppressWarnings(as.logical(x$isgeneral))

  if(is.null(from)) from <- dates[which.min(dates)]

  update_dates <- seq(from, to, by = 1)
  update_dates <- update_dates[!update_dates %in% dates]
  if(length(update_dates) == 0) {
    message("Already up to date.")
    return(invisible(TRUE))
  }

  ctad <- covidtracker_actions_dates(countries, update_dates, verbose = TRUE)
  ctad <- ctad[!unlist(lapply(ctad, is.null))]
  ctad <- data_frame_actions(ctad)
  ctad <- ctad[,colnames]

  x <- rbind(x, ctad)
  x <- x[order(x$country_code, x$date, x$policy_type_code),]
  write.csv(x, file = file, row.names = FALSE)
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
  cta <- data_frame_actions(x = cta)
  cta <- cta[order(cta$country_code, cta$date, cta$policy_type_code),]
  write.csv(cta, file = file, row.names = FALSE)

}

#' @rdname covidtracker_update_actions_csv
#' @param parse_factors pars ordinal variables to factors
#' @export
covidtracker_csv_v4 <- function(countries = NULL, parse_factors = FALSE){
  checkmate::assert_character(countries, pattern = "[A-Za-z]{3}", null.ok = TRUE)
  checkmate::assert_flag(parse_factors)
  url <- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"
  r <- httr::GET(url)
  c <- httr::content(r)
  col_spec <- readr::cols(
    .default = readr::col_double(),
    CountryName = readr::col_character(),
    CountryCode = readr::col_character(),
    Date = readr::col_character()
  )
  od <- suppressWarnings(httr::content(r, type = "text/csv", encoding = "UTF-8", col_types = col_spec))
  od$X27 <- NULL
  od$Date <- as.Date(od$Date, format = "%Y%m%d")
  if(!is.null(countries)) od <- od[od$CountryCode %in% countries,]

  if(parse_factors){
    ml <- measure_ordinal_labels_v4()
    od_nms <- unique(paste(ml$variable, ml$names, sep = "_"))
    for(i in seq_along(od_nms)){
      variable <- strsplit(od_nms[i], split = "_")[[1]][1]
      od[[od_nms[i]]] <- factor(od[[od_nms[i]]],
                                levels = ml[ml$variable == variable, "code"],
                                labels = ml[ml$variable == variable, "label"])
    }
  }

  od
}



measure_ordinal_labels_v4 <- function(){
  ml <- list()
  ml[[1]] <-
    data.frame(variable = c(rep("S1", 3)),
               names = c(rep("School closing", 3)),
               code = c(0, 1, 2),
               label = c("No measures",
                         "Recommend closing",
                         "Require closing"),
               stringsAsFactors = FALSE)
  ml[[2]] <-
    data.frame(variable = c(rep("S2", 3)),
               names = c(rep("Workplace closing", 3)),
               code = c(0, 1, 2),
               label = c("No measures",
                         "Recommend closing",
                         "Require closing"),
               stringsAsFactors = FALSE)
  ml[[3]] <-
    data.frame(variable = c(rep("S3", 3)),
               names = c(rep("Cancel public events", 3)),
               code = c(0, 1, 2),
               label = c("No measures",
                         "Recommend cancelling",
                         "Require cancelling"),
               stringsAsFactors = FALSE)
  ml[[4]] <-
    data.frame(variable = c(rep("S4", 3)),
               names = c(rep("Close public transport", 3)),
               code = c(0, 1, 2),
               label = c("No measures",
                         "Recommend closing",
                         "Require closing"),
               stringsAsFactors = FALSE)
  ml[[5]] <-
    data.frame(variable = c(rep("S5",2)),
               names = c(rep("Public information campaigns", 2)),
               code = c(0, 1),
               label = c("No COVID-19 public information campaign",
                         "COVID-19 public information campaign"),
               stringsAsFactors = FALSE)
  ml[[6]] <-
    data.frame(variable = c(rep("S6",3)),
               names = c(rep("Restrictions on internal movement", 3)),
               code = c(0, 1, 2),
               label = c("No measures",
                         "Recommend movement restriction",
                         "Restrict movement"),
               stringsAsFactors = FALSE)
  ml[[7]] <-
    data.frame(variable = c(rep("S7",4)),
               names = c(rep("International travel controls",4)),
               code = c(0, 1, 2, 3),
               label = c("No measures",
                         "Screening",
                         "Quarantine on high-risk regions",
                         "Ban on high-risk regions"),
               stringsAsFactors = FALSE)
  ml[[8]] <-
    data.frame(variable = c(rep("S12",4)),
               names = c(rep("Testing framework",4)),
               code = c(0, 1, 2, 3),
               label = c("No testing policy",
                         "Only testing those who both (a) have symptoms, and (b) meet specific criteria",
                         "Testing of anyone showing COVID19 symptoms",
                         "Open public testing"),
               stringsAsFactors = FALSE)
  ml[[9]] <-
    data.frame(variable = c(rep("S13",3)),
               names = c(rep("Contact tracing",3)),
               code = c(0, 1, 2),
               label = c("No contact tracing",
                         "Limited contact tracing",
                         "Comprehensive contact tracings"),
               stringsAsFactors = FALSE)
  do.call(rbind, ml)
}


#' @rdname covidtracker_update_actions_csv
#' @param parse_factors pars ordinal variables to factors
#' @export
covidtracker_csv_v5 <- function(countries = NULL, parse_factors = FALSE){
  checkmate::assert_character(countries, pattern = "[A-Za-z]{3}", null.ok = TRUE)
  checkmate::assert_flag(parse_factors)
  url <- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"
  r <- httr::GET(url)
  c <- httr::content(r)
  col_spec <- readr::cols(
    .default = readr::col_double(),
    CountryName = readr::col_character(),
    CountryCode = readr::col_character(),
    Date = readr::col_character()
  )
  od <- suppressWarnings(httr::content(r, type = "text/csv", encoding = "UTF-8", col_types = col_spec))
  od$X27 <- NULL
  od$Date <- as.Date(od$Date, format = "%Y%m%d")
  if(!is.null(countries)) od <- od[od$CountryCode %in% countries,]

  if(parse_factors){
    ml <- measure_ordinal_labels_v5()
    od_nms <- unique(paste(ml$variable, ml$names, sep = "_"))
    for(i in seq_along(od_nms)){
      variable <- strsplit(od_nms[i], split = "_")[[1]][1]
      od[[od_nms[i]]] <- factor(od[[od_nms[i]]],
                                levels = ml[ml$variable == variable, "code"],
                                labels = ml[ml$variable == variable, "label"])
    }
  }
  od
}

measure_ordinal_labels_v5 <- function(){
  ml <- list()
  ml[[length(ml) + 1]] <-
    data.frame(variable = c(rep("C1", 3)),
               names = c(rep("School closing", 3)),
               code = c(0, 1, 2),
               label = c("No measures",
                         "Recommend closing",
                         "Require closing"),
               stringsAsFactors = FALSE)
  ml[[length(ml) + 1]] <-
    data.frame(variable = c(rep("C2", 3)),
               names = c(rep("Workplace closing", 3)),
               code = c(0, 1, 2),
               label = c("No measures",
                         "Recommend closing",
                         "Require closing"),
               stringsAsFactors = FALSE)
  ml[[length(ml) + 1]] <-
    data.frame(variable = c(rep("C3", 3)),
               names = c(rep("Cancel public events", 3)),
               code = c(0, 1, 2),
               label = c("No measures",
                         "Recommend cancelling",
                         "Require cancelling"),
               stringsAsFactors = FALSE)
  ml[[length(ml) + 1]] <-
    data.frame(variable = c(rep("C4", 5)),
               names = c(rep("Restrictions on gatherings", 5)),
               code = c(0, 1, 2, 3, 4),
               label = c("No measures",
                         "> 1000 people",
                         "100-1000 people",
                         "10-100 people",
                         "< 10 people"),
               stringsAsFactors = FALSE)
  ml[[length(ml) + 1]] <-
    data.frame(variable = c(rep("C5", 3)),
               names = c(rep("Close public transport", 3)),
               code = c(0, 1, 2),
               label = c("No measures",
                         "Recommend closing",
                         "Require closing"),
               stringsAsFactors = FALSE)
  ml[[length(ml) + 1]] <-
    data.frame(variable = c(rep("C6",4)),
               names = c(rep("Stay at home requirements", 4)),
               code = c(0, 1, 2, 3),
               label = c("No measures",
                         "Recommend not leaving house",
                         "Require not leaving house",
                         "Require not leaving house (minimal exceptions)"),
               stringsAsFactors = FALSE)
  ml[[length(ml) + 1]] <-
    data.frame(variable = c(rep("C7",3)),
               names = c(rep("Restrictions on internal movement", 3)),
               code = c(0, 1, 2),
               label = c("No measures",
                         "Recommend movement restriction",
                         "Restrict movement"),
               stringsAsFactors = FALSE)
  ml[[length(ml) + 1]] <-
    data.frame(variable = c(rep("C8",4)),
               names = c(rep("International travel controls",4)),
               code = c(0, 1, 2, 3),
               label = c("No measures",
                         "Screening",
                         "Quarantine on high-risk regions",
                         "Ban on high-risk regions"),
               stringsAsFactors = FALSE)
  ml[[length(ml) + 1]] <-
    data.frame(variable = c(rep("H1",3)),
               names = c(rep("Public information campaigns", 3)),
               label = c("No COVID-19 public information campaign",
                         "Public officials urging caution about COVID-19",
                         "Coordinated COVID-19 public information campaign"),
               code = c(0, 1, 2),
               stringsAsFactors = FALSE)
  ml[[length(ml) + 1]] <-
    data.frame(variable = c(rep("H2",4)),
               names = c(rep("Testing policy",4)),
               code = c(0, 1, 2, 3),
               label = c("No testing policy",
                         "Only testing those who both (a) have symptoms, and (b) meet specific criteria",
                         "Testing of anyone showing COVID19 symptoms",
                         "Open public testing"),
               stringsAsFactors = FALSE)
  ml[[length(ml) + 1]] <-
    data.frame(variable = c(rep("H3",3)),
               names = c(rep("Contact tracing",3)),
               code = c(0, 1, 2),
               label = c("No contact tracing",
                         "Limited contact tracing",
                         "Comprehensive contact tracings"),
               stringsAsFactors = FALSE)
  do.call(rbind, ml)
}
