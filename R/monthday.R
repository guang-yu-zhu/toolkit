#' Extract and Format Month and Day
#'
#' This function takes a date input, extracts the month and day, and formats them as a string.
#'
#' @param date A Date object or a character string convertible to a Date. The date to extract month and day from.
#' @return A character string combining the abbreviated month and day (e.g., "Jan 1").
#' @examples
#' monthday(as.Date("2024-01-01"))
#' monthday(Sys.Date())
#' @importFrom lubridate month day
#' @export
monthday <- function(date) {
  m <- month(date, label = TRUE, abbr = TRUE)
  d <- day(date)
  paste(m, d)
}