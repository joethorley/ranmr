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

plural <- function (x, s = FALSE, end = "") {
  paste0(x, ifelse(s, "s", ""), end)
}

punctuate_strings <- function (x, qualifier = "or") {
  if(length(x) == 1)
    return (x)
  n <- length(x)
  paste(paste(x[-n], collapse = ", "), qualifier, x[n])
}
