

#' Download Latest Covid Dataset
#'
#' Download the latest dataset from the John Hopkins website
#'
#'
#' @return data.table of confirmed cases by country and date
#' @export
#'
get_confirmed_cases <- function() {

  ##URL of Latest Dataset (Updated Daily)
  cases_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

  confirmed_cases <- data.table::fread(cases_url)

  return(confirmed_cases)
}

#' Tidy the Raw John Hopkins Data
#'
#' Tidy the Raw Data and Add the latest Stats for easy filtering
#'
#' @param raw_data
#'
#' @return tidy dataset in long format
#' @export
#'
tidy_confirmed_cases <- function(raw_data){

  data <- reshape2::melt(raw_data[,c(-1,-3,-4)], id.var="Country/Region")

  data <- data[, .( value.date = as.Date(variable, "%m/%d/%y"),`Country/Region`,value)]
  #prune the data so that we only have the data for each country since the first reported case
  data[, value:=ifelse(value==0, NA, value)]
  data <- data[!is.na(data$value)]

  ##Data is split out at province/region detail for some countries so we summarise the data by Country. This will be our lower level of granularity for analysis
  data <- data[, .(value=sum(value)), .(`Country/Region`, value.date)]
  data.table::setkey(data, 'Country/Region')

  #Stats to enable filtering later
  stats <- data[,.(first.case=first(value.date), latest.value=last(value)), .(`Country/Region`)]
  setkey(stats, 'Country/Region')



  #Append the Stats to the cleaned John Hopkins Data
  confirmed_cases <- stats[data]

  return(confirmed_cases)
}


#' Get latest dataset for a country
#'
#' Long form dataset showing the daily tallies of confirmed cases since the first case was reported in the country.
#' Also shows the latest value and the first case in that country. These columns are useful for pivoting data from multiple countries
#'
#' @param country name of Country/Region
#'
#' @return
#' @export
#'
get_latest_country_data <- function(country) {
  data <- tidy_confirmed_cases(get_confirmed_cases())[country]
  return(data)
}

