fill_in_ages_fish <- function(x) {
  assert_that(!is.unsorted(x$Date))
  age <- x$Age
  year <- lubridate::year(x$Date)
  newage <- age[1] + year - year[1]
  age[is.na(age)] <- newage[is.na(age)]
  if (!identical(age, newage)) {
    warning("inconsistent ages fish ", as.character(x$Fish[1]), call. = FALSE)
  }
  x$Age <- newage
  x
}

#' Fill In Ages
#'
#' @param x The data.frame with missing ages.
#'
#' @return The modified data frame.
#' @export
#' @examples
#' ferox <- ranmrdata::ferox
#' fill_in_ages(ferox@data)
fill_in_ages <- function(x) {
  datacheckr::check_data1(x, values = list(
    Date = Sys.Date(),
    Fish = factor(1),
    Age = c(1L, NA)))

  x %<>% plyr::ddply(.variables = ("Fish"), fill_in_ages_fish)
  x
}
