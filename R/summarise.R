#' Summarise Mark-Recapture Data
#'
#' Summarises mark-recapture data as a named list.
#' The data must be in the same format as the \code{\link{ferox}}
#' data provided with this package (only the Fish and Date columns are required).
#'
#' @param x A data.frame of the mark-recapture data to summarise.
#' @return A named list of the summary values.
#' @seealso \code{\link{ranmr}}
#' @export
#' @examples
#' summarise_mr()
summarise_mr <- function (x = ferox()) {
  assert_that(is.data.frame(x))
  check_rows(x)
  check_columns(x, c("Fish", "Date"))

  x <- process_mr_data(x, FALSE)
  y <- list()
  y$nFish <- nlevels(x$Fish)
  y$nIntraRecaptures <- sum(x$IntraRecapture)
  x <- dplyr::filter_(x, ~!IntraRecapture)
  y$nRecapFish <- length(unique(x$Fish[x$InterRecapture]))
  y$nInterRecaptures <- sum(x$InterRecapture)
  x$Year <- lubridate::year(x$Date)
  x$Month <- lubridate::month(x$Date)
  x <- dplyr::select_(x, ~-Fish, ~-Date, ~-IntraRecapture, ~-InterRecapture)
  minmax <- function (x) {
    x <- range(x, na.rm = TRUE)
    names(x) <- c("min", "max")
    x
  }
  y <- c(y, lapply(x, minmax))
  y
}
