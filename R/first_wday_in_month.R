
#' Find the first X day in month Y in one or multiple years
#'
#' Find, for example, the first Saturday in November every year from 2014-2018.
#'
#' @param day A day of the week (e.g., "Wednesday", "Thursday", etc.)
#' @param day A month of the year (e.g., "November", "December", etc.)
#' @param years A vector of years (e.g., 2014, 2014:2016)
#' @return A vector of dates
#' @examples
#' # Find the first Saturday of November in 2014-2018
#' first_sats <- first_wday_in_month(day="Saturday", month="November", years=2014:2018)
#' first_sats
#' @export
first_wday_in_month <- function(day, month, years){
  first_days <- lubridate::ymd()
  for(i in 1:length(years)){
    year <- years[i]
    first7days <- paste0(month, " 1, ", year) %>% lubridate::mdy() %>% seq(., by="day", length.out = 7)
    firstXday <- first7days[which(weekdays(first7days)==day)] %>% lubridate::ymd()
    first_days[i] <- firstXday
  }
  return(first_days)
}




