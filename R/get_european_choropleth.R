#' Prepare a ggplot2-based choropleth on European national (NUTS0) countries.
#' @param input_data A data frame containing the data to be mapped. It should contain two columns: the first containing the NUTS code of the region,
#' the second the value that should be mapped.
#' @return A ggplot2 object
#' @section Details
#' The country codes should be a NUTS (0-2) code, an alphanumeric code where the first two letters are almost identical to ISO 3166-1 alpha-2 code. The only exceptions are \code{UK} instead of \code{GB} and \code{EL} instead of \code{GR}. 
#' @export
#'
get_european_choropleth <- function(input_data) {
  #### Load shapefiles
  eumap_0_2 = read_rds(system.file("eumap_merged_0_2_2016.rds", package = "panas"))
  #### Check the input data frame
  if (!is.data.frame(input_data)) {
    stop('The input_data is not a data frame')
  } else if (ncol(input_data)!=2) {
    stop('Input data frame has the wrong number of columns (!=2) ')
  }
  #### Homogenise the column names
  colnames(input_data) = c('area','value')
  ## Remove unneeded regions (to speed up the plot)
  if (max(stringr::str_length(input_data$area)) == 2) {
    eumap_0_2 = filter(eumap_0_2, stringr::str_length(id) == 2)
  } else if (max(stringr::str_length(input_data$area)) == 3) {
    eumap_0_2 = filter(eumap_0_2, stringr::str_length(id) <= 3)
  }
  ## Match EUMAP ADM0 names
  input_data = mutate(input_data, area = if_else(area == 'GR', 'EL', area)) %>%
    mutate(area = if_else(area == 'GB', 'UK', area))
  
  jj = left_join(eumap_0_2, input_data, by = c('id' = 'area')) %>%
    mutate(level = stringr::str_length(id))
  
  g = ggplot(jj, aes(x = long, y = lat, group = group)) +
    borders(database = 'world', regions = '.', fill = 'lightgrey', size = 0.1) +
    geom_polygon(color = 'lightgray', size = 0.1, fill = 'lightgrey') +
    geom_polygon(data = filter(jj, !is.na(value)), aes(fill = value), color = 'lightgrey', size = 0.1) +
    geom_path(data = filter(jj, level == 2), color = 'gray50', size = 0.1) +
    coord_quickmap(ylim = c(35, 65), xlim = c(-15, 25)) +
    xlab('Longitude') + ylab('Latitude') 
  
  return(g)
}
