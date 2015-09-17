fill_in_ages <- function(x) {
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

#' Ferox Trout Mark-Recapture Data
#'
#' @return A data.frame of ferox trout mark-recapture data from
#' the \code{ranmrdata} package.
#' @seealso \code{ranmrdata::\link[ranmrdata]{ferox}}
#' @export
#' @examples
#' ferox()
ferox <- function() {
  data(list = "ferox", package = "ranmrdata", envir = environment())

  ferox %<>% plyr::ddply(.variables = ("Fish"), fill_in_ages)
  ferox
}
