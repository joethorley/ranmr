#' Save KML File
#'
#' Saves mark-recapture data as a KML file for viewing in google earth
#' where consecutive
#' encounters with the same individual are joined by lines.
#' The data must be in the same format as the \code{\link{ferox}}
#' data provided with this package.
#'
#' @param x A data.frame of the mark-recapture data to plot.
#' @param file A string of the file name.
#' @seealso \code{\link{ranmr}}
#' @export
save_kml <- function(x, file = "ferox.kml") {
  if (!requireNamespace("plotKML"))
    stop("package plotKML required")

  assert_that(is.data.frame(x))
  assert_that(is.string(file))

  . <- NULL

  check_rows(x)
  check_columns(x, c("Date", "Fish", "Latitude", "Longitude"))

  file %<>% file.path(getOption("ranmr.dir", "results"), .)

  x <- process_mr_data(x)

  z <- dplyr::filter_(x, ~InterRecapture)
  x %<>% dplyr::filter_(~!InterRecapture)

  x <- sp::SpatialPointsDataFrame(dplyr::select_(x,~Longitude, ~Latitude),
                                  dplyr::select_(x,~-Latitude, ~-Longitude),
                                  proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

  z <- sp::SpatialPointsDataFrame(dplyr::select_(z,~Longitude, ~Latitude),
                                  dplyr::select_(z,~-Latitude, ~-Longitude),
                                  proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

  plotKML::kml_open(file.name = file)
  on.exit(plotKML::kml_close(file.name = file))
  plotKML::kml_layer(x, subfolder.name = "capture", shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png", points_names = x$Fish)
  plotKML::kml_layer(z, subfolder.name = "recapture", shape = "http://maps.google.com/mapfiles/dir_0.png",
                     points_names = z$Fish)

  invisible()
}
