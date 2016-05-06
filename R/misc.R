process_mr_data <- function (x, rm_intra_recaps = TRUE) {
  assert_that(is.data.frame(x))
  assert_that(is.flag(rm_intra_recaps) && noNA(rm_intra_recaps))

  check_rows(x)

  check_columns(x, c("Fish", "Date"))
  check_class_columns(x, list("Fish" = "factor",
                              "Date" = "Date"))
  x <- dplyr::arrange_(x, "Date", "Fish")
  x$InterRecapture <- duplicated(x$Fish)
  x$IntraRecapture <- duplicated(paste(x$Fish, lubridate::year(x$Date)))
  if(rm_intra_recaps) {
    x <- dplyr::filter_(x, ~!IntraRecapture)
    x$IntraRecapture <- NULL
  }
  x
}

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
#' summarise_mr(ranmrdata::ferox@data)
summarise_mr <- function (x) {
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

#' Tabulate Mark-Recapture Data
#'
#' Tabulates mark-recapture data as a matrix.
#' The data must be in the same format as the \code{\link{ferox}}
#' data provided with this package (only the Fish and Date columns are required).
#'
#' @param x A data.frame of the mark-recapture data to tabulate.
#' @return A matrix of the numbers of captures and recaptures by year.
#' @seealso \code{\link{ranmr}}
#' @export
#' @examples
#' tabulate_mr(ranmrdata::ferox@data)
tabulate_mr <- function(x) {
  assert_that(is.data.frame(x))
  check_rows(x)
  check_columns(x, c("Fish", "Date"))

  x <- process_mr_data(x)
  x$Year <- lubridate::year(x$Date)
  x$Year <- factor(x$Year, levels = min(x$Year):max(x$Year))

  y <- dplyr::filter_(x, ~!InterRecapture)
  y$InitialYear <- y$Year
  y <- dplyr::select_(y, ~Fish, ~InitialYear)
  x <- dplyr::inner_join(x, y, by = "Fish")

  x <- dplyr::summarise_(dplyr::group_by_(x, ~InitialYear, ~Year), ~n())

  z <- matrix(0, nrow = nlevels(x$Year), ncol = nlevels(x$Year),
              dimnames = list(levels(x$Year), levels(x$Year)))

  x$Year <- as.integer(x$Year)
  x$InitialYear <- as.integer(x$InitialYear)
  for (i in 1:nrow(x)) {
    z[x$InitialYear[i], x$Year[i]] <- x[["n()"]][i]
  }
  d <- diag(z)
  is.na(z[lower.tri(z, diag = TRUE)]) <- TRUE
  z[,1] <- d
  colnames(z)[1] <- "Captures"
  z <- cbind(as.integer(row.names(z)), z)
  colnames(z)[1] <- "Year"
  z
}
