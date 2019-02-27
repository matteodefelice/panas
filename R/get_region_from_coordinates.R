#' Return the geographci region of a set of coordinates according to administrative boundaries.
#'
#' The function defines in which region a set of coordinates (stored into a \code{data.frame}) lie
#' The administrative boundaries can be chosen from a set of pre-definites shapefiles.

#' @param obj A data frame of coordinates stored using lat/lon coordinates. See Details section for further details.
#' @param shapefile The shapefile to be used to aggregate the grid points. 
#' @return A vector containing all the \code{shapefile_id_field}
#' @author Matteo De Felice
#' @export
#' @details Details
#' \code{obj} must be a data frame containing two variables: 
#' 1. Latitude: a double in the range -90, 90 named as lat, Lat, latitude, latit, etc.
#' 2. Longitude: a double in the range -180, 180 named as lon, Lon, longitude, long, etc.
#'
#' The shapefiles available are the following:
#' \itemize{
#' \item \code{NUTS0-2}: Data from EUROSTAT NUTS (small islands removed) https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nutscountries_EU 
#' \item \code{eh2050}: cluster as defined in the FP7 e-Highway2050 project
#' \item \code{hybas05}: HydroBASINS Level 5 (http://www.hydrosheds.org/page/hydrobasins)
#' \item \code{hybas06}: HydroBASINS Level 6 (http://www.hydrosheds.org/page/hydrobasins)
#' \item \code{WAPP}: WAPP catchments from the JRC LISFLOOD hydrological model
#' }

get_region_from_coordinates <- function(obj, shapefile = 'NUTS0', path_to_shapefile = NULL) {
  
  if (shapefile == 'NUTS2') {
    eumap = read_rds(system.file("NUTS_RG_01M_2016_4326_LEVL_2.reduced.rds", package = "panas"))
    shapefile_id_field = 'NUTS_ID'
  } else if (shapefile == 'NUTS1') {
    eumap = read_rds(system.file("NUTS_RG_01M_2016_4326_LEVL_1.reduced.rds", package = "panas"))
    shapefile_id_field = 'NUTS_ID'
  } else if (shapefile == 'NUTS0'){
    eumap = read_rds(system.file("NUTS_RG_01M_2016_4326_LEVL_0.reduced.rds", package = "panas"))
    shapefile_id_field = 'NUTS_ID'
  } else if (shapefile == 'eh2050') {
    eumap = read_rds(system.file("eh2050_clusters.rds", package = "panas"))
    shapefile_id_field = 'NUTS_ID'
  } else if (shapefile == 'hybas05') {
    eumap = read_rds(system.file("hybas_lev05.rds", package = "panas"))
    shapefile_id_field = 'HYBAS_ID'
  } else if (shapefile == 'hybas06') {
    eumap = read_rds(system.file("hybas_lev06.rds", package = "panas"))
    shapefile_id_field = 'HYBAS_ID'
  } else if (shapefile == 'WAPP') {
    eumap = read_rds(system.file("wapp_catchments.rds", package = "panas"))
    shapefile_id_field = 'name'
  } else {
    stop('Shape option not existent')
  }
  
  # Check if obj is a well-formed data frame
  if (!is.data.frame(obj)) {
    stop("Obj must be a data frame.  ")
  } else if (ncol(obj) != 2) {
    stop("Obj must have two columns")
  } else if (!any(str_detect(str_to_lower(names(obj)), 'lat'))) {
    stop('One of the two variables in obj must contain lat')
  } else if (!any(str_detect(str_to_lower(names(obj)), 'lon'))) {
    stop('One of the two variables in obj must contain lon')
  }
  
  # Create a canonical data frame (lat, lon)
  names(obj) = str_to_lower(names(obj))
  if (str_detect(names(obj)[1], 'lat')) {
    names(obj) = c('lat', 'lon')
  } else {
    names(obj) = c('lon', 'lat')
  }
  # Check lon
  if (any(obj$lon > 180)) {
    obj$lon[obj$lon > 180] = obj$lon[obj$lon > 180] - 360
  }
  # Convert pts to a Spatial object
  coordinates(obj) = c("lon", "lat")
  # CHECK if all shapefiles have proj4string
  proj4string(obj) = proj4string(eumap)
  # Calculate the spatial overlay of pts points over the shapefile
  over_target = over(obj, as(eumap, "SpatialPolygons"))
  
  return(eumap[[shapefile_id_field]][over_target])
}