#' Aggregate a gridded object into national or regional boundaries.
#'
#' The function spatially aggregate using a function (for example, \code{mean}) all the grid points
#' within the borders defined by a shapefile. The function already provides some shapefiles for
#' Europe.

#' @param obj A gridded object: a grid structure from Climate4R functions or a \code{list} with \code{lat}, \code{lon} and \code{data} fields. See Details section for further details.
#' @param aggregate_function The function to be used to aggregate the grid points. Options are: \code{mean} and \code{sum}
#' @param shapefile The shapefile to be used to aggregate the grid points. The options are: \code{NUTS0}, \code{NUTS1}, \code{NUTS2}, \code{eh2050} and \code{custom}
#' @param shapefile_id_field Field name of the shapefile to be used as identifier
#' @param path_to_shapefile The path of the \code{.shp} file to be used when \code{shapefile} is set to \code{custom}
#' @param cos_weighted Define is the grid points will be weighted according the cosine of the latitude
#' @return A list containing all the fields names ad \code{shapefile_id_field}, each field contains aggregated the time-series
#' @author Matteo De Felice
#' @export
#' @details Details
#' The \code{obj} can be: 1. a list with three mandatory fields: \code{lat} with the latitude values, \code{lon} with the longitude and \code{data} with the gridded field consistent with the coordinates;
#' 2. a grid structure as in the \code{Climate4R} bundle (citation neeeded), for example from \code{loadeR} package.
#'
#' The shapefiles available are the following:
#' - \code{NUTS0}: countries_EU (specify source)
#' - \code{NUTS1}: NUTS_REG_01M_2013_ADM1 (specify source)
#' - \code{NUTS2}: NUTS_REG_01M_2013_REGIONS (specify source)
#' - \code{eh2050}: cluster as defined in the FP7 e-Highway2050 project
get_ts_from_shp <- function(obj, aggregate_function = 'mean', shapefile = 'NUTS0', shapefile_id_field = 'NUTS_ID', path_to_shapefile = NULL, cos_weighted = TRUE) {

  if (shapefile == 'NUTS2') {
    eumap = readOGR(system.file("NUTS", package = "panas"), "NUTS_REG_01M_2013_REGIONS")
  } else if (shapefile == 'NUTS1') {
    eumap = readOGR(system.file("NUTS", package = "panas"), "NUTS_REG_01M_2013_ADM1")
  } else if (shapefile == 'NUTS0'){
    eumap = readOGR(system.file("NUTS", package = "panas"), "countries_EU")
    names(eumap)[1] = "NUTS_ID"
  } else if (shapefile == 'eh2050') {
    eumap = readOGR(system.file("NUTS", package = "panas"), "borders-wgs84")
  } else if (shapefile == 'custom'){
    stop('TBD')
  } else {
    stop('Shape option not existent')
  }

  # Select type of arithmetic function
  if (aggregate_function == 'mean') {
    base_fun = mean
    array_fun = rowMeans
  } else if (aggregate_function == 'sum') {
    base_fun = sum
    array_fun = rowSums
  } else {
    stop('Aggregate function not recognised')
  }
  # Check if obj is a well-formed list and extract the information about data and coordinates
  if (!is.list(obj)) {
    stop("Obj must be a list. ")
  } else if (prod(c('lat', 'lon', 'data') %in% names(d)) == 1) {
    pts = expand.grid(lat = lat, lon = lon)
    pts_index = expand.grid(lat = seq(1, length(lat)), lon = seq(1, length(lon)))
    obj = obj$data
  } else if (prod(c('Data', 'xyCoords') %in% names(d)) == 1) {
    pts = expand.grid(lat = obj$xyCoords$y, lon = obj$xyCoords$x)
    pts_index = expand.grid(lat = seq(1, length(obj$xyCoords$y)), lon = seq(1, length(obj$xyCoords$x)))
    lat = obj$xyCoords$y
    lon = obj$xyCoords$x
    obj = obj$Data
  } else {
    stop('Obj is not a well-formed list. See documentation for more details. ')
  }

  # Convert pts to a Spatial object
  coordinates(pts) = c("lon", "lat")
  # CHECK if all shapefiles have proj4string
  proj4string(pts) = proj4string(eumap)
  # Calculate the spatial overlay of pts points over the shapefile
  over_target = over(pts, as(eumap, "SpatialPolygons"))
  pts$region = eumap[[shapefile_id_field]][over_target]

  pts_index$region = droplevels(eumap$NUTS_ID[over_target])
  pts_index = pts_index[!is.na(over_target), ]

  # For each region compute the aggregation
  SEL_REGIONS = levels(pts_index$region)
  data = list()
  for (REG in SEL_REGIONS) {
    # Select all the points in REG
    sel_pts = pts_index[pts_index$region == REG, c(1, 2)]
    lsel = vector("list", nrow(sel_pts))
    weight_lat = 0
    for (i in 1:nrow(sel_pts)) {
      if (length(dim(obj)) == 2) {
        ## 2D array
        lsel[[i]] = obj[sel_pts$lat[i], sel_pts$lon[i]]
      } else if (length(dim(obj)) == 3) {
        ## 3D array
        lsel[[i]] = obj[, sel_pts$lat[i], sel_pts$lon[i]]
      } else {
        ## 4D array
        lsel[[i]] = t(obj[, , sel_pts$lat[i], sel_pts$lon[i]])
      }
      if (cos_weighted) {
        # Weight by cos(lat)
        lsel[[i]] = lsel[[i]] * cos(sel_pts$lat[i] * pi / 180)
        weight_lat =  weight_lat + cos(sel_pts$lat[i] * pi / 180)
      }
    }
    lsel = do.call("cbind", lsel)
    if ((aggregate_function != 'sum') && (aggregate_function != 'mean')) {
      # In case of != sum or mean
      # we assume the output is "just" the raw
      # cbind of selected points
      d = lsel
    } else {
      if (length(dim(obj)) == 2) {
        d = base_fun(lsel, na.rm = T)
      } else if (length(dim(obj)) == 3) {
        d = array_fun(lsel, na.rm = T)
      } else {
        nmem = dim(obj)[1]
        d = matrix(NA, nrow = nrow(lsel), ncol = nmem)
        for (k in 1:nmem) {
          d[, k] = array_fun(matrix(lsel[, seq(k, ncol(lsel), nmem)], nrow = nrow(lsel)), na.rm = T)
        }
      }
      # cos_weighted part
      if (cos_weighted && aggregate_function == 'mean') {
        d = d * nrow(sel_pts) / weight_lat
      }
    }
    data[[REG]] = d
  }
  return(data)
}
