#' Aggregate a gridded object into national or regional boundaries.
#'
#' The function spatially aggregates using a function (for example, \code{mean}) all the grid points
#' within the borders defined by a shapefile. The function provides some shapefiles for
#' Europe.

#' @param obj A gridded object: a grid structure from Climate4R functions or a \code{list} with \code{lat}, \code{lon} and \code{data} fields. See Details section for further details.
#' @param aggregate_function The function to be used to aggregate the grid points. Options are: \code{mean} and \code{sum}
#' @param shapefile The shapefile to be used to aggregate the grid points. 
#' @param cos_weighted Define if the grid points will be weighted according the cosine of the latitude
#' @return A list containing all the fields names ad \code{shapefile_id_field}, each field contains aggregated the time-series
#' @author Matteo De Felice
#' @keywords internal
#' @details Details
#' The \code{obj} must be a list with three mandatory fields: \code{lat} with the latitude values, \code{lon} with the longitude and \code{data} with the gridded field consistent with the coordinates;
#'
#' The shapefiles available are the following:
#' \itemize{
#' \item \code{NUTS0-2}: Data from EUROSTAT NUTS (small islands removed) https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nutscountries_EU 
#' \item \code{eh2050}: cluster as defined in the FP7 e-Highway2050 project
#' \item \code{hybas05}: HydroBASINS Level 5 (http://www.hydrosheds.org/page/hydrobasins)
#' \item \code{hybas06}: HydroBASINS Level 6 (http://www.hydrosheds.org/page/hydrobasins)
#' \item \code{WAPP}: WAPP catchments from the JRC LISFLOOD hydrological model
#' }

get_ts_from_shp.data.frame <- function(obj, aggregate_function = 'mean', shapefile = 'NUTS0', path_to_shapefile = NULL, cos_weighted = TRUE) {
  
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
  if (!is.data.frame(obj)) {
    stop("Obj must be a data frame. ")
  } else if (prod(c('lat', 'lon', 'data') %in% names(obj)) == 1) {
    pts = obj[,c('lat', 'lon')] #expand.grid(lat = obj$lat, lon = obj$lon)
    if (!('time' %in% names(obj))) {
      obj$time = 1
    }
  } else {
    stop('Obj is not a well-formed data frame. Have you checked the column names? See documentation for more details. ')
  }
  
  # Convert pts to a Spatial object
  coordinates(pts) = c("lon", "lat")
  # CHECK if all shapefiles have proj4string
  proj4string(pts) = proj4string(eumap)
  # Calculate the spatial overlay of pts points over the shapefile
  over_target = over(pts, as(eumap, "SpatialPolygons"))
  
  obj$region = eumap[[shapefile_id_field]][over_target]
  
  # For each region compute the aggregation
  SEL_REGIONS = unique(obj$region)
  SEL_REGIONS = SEL_REGIONS[!is.na(SEL_REGIONS)]
  
  data = list()
  for (REG in SEL_REGIONS) {
    # Select all the points in REG
    sel_pts = obj %>% 
      dplyr::filter(region == REG)
    lsel = vector("list", nrow(sel_pts))
    weight_lat = 0
    cum_weight_mat = 0 # accumulator for weight matrix
    
    for (i in 1:nrow(sel_pts)) {
      
      sel_pts$data_out[i] = sel_pts$data[i]
      
      if (cos_weighted && aggregate_function == 'mean') {
        # Weight by cos(lat)
        sel_pts$data_out[i] = sel_pts$data_out[i] * cos(sel_pts$lat[i] * pi / 180)
        sel_pts$weight_lat[i] = cos(sel_pts$lat[i] * pi / 180)
      } else {
        sel_pts$weight_lat[i]  = 1
      }
    }
    # lsel = do.call("cbind", lsel)
    
    if ((aggregate_function != 'sum') && (aggregate_function != 'mean')) {
      # In case of != sum or mean
      # we assume the output is "just" the raw
      # cbind of selected points
      d = sel_pts %>%
        select(lat, lon, time, data)
    } else {
      
      d_out = sel_pts %>%
        group_by(time) %>%
        summarise(
          data = base_fun(data * weight_lat, na.rm = TRUE) / mean(weight_lat)
        ) %>%
        arrange(
          time
        ) 
      d = d_out$data
    }
    
    data[[REG]] = d
  }
  return(data)
}
