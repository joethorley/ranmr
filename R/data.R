#' Ferox Trout Mark-Recapture Data
#'
#' @return A data.frame of ferox trout mark-recapture data from
#' the \code{ranmrdata} package.
#' @seealso \code{ranmrdata::\link[ranmrdata]{ferox}}
#' @export
#' @examples
#' ferox()
ferox <- function () {
  data(list = "ferox", package = "ranmrdata", envir = environment())
  ferox
}
