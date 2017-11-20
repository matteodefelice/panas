#' Prepare a ggplot2-based choropleth on European national (NUTS0) countries.
#' @param input_data A data frame containing the data to be mapped. It should contain two columns: the first containing the ISO ALPHA-2
#' code of the country, the second the value that should be mapped.
#' @return A ggplot2 object
#' @section Details
#' The country codes should be a NUTS code, a two-letter almost identical to ISO 3166-1 alpha-2 code. The only exceptions are \code{UK} instead of \code{GB} and \code{EL} instead of \code{GR}. 
#' @export
#'
get_european_choropleth <- function(input_data) {
  #### Load shapefiles
  load(system.file("NUTS", "eumap_adm0.rda", package = "panas"))
  #### Check the input data frame
  if (!is.data.frame(input_data)) {
    stop('The input_data is not a data frame')
  } else if (ncol(input_data)!=2) {
    stop('Input data frame has the wrong number of columns (!=2) ')
  }
  #### Homogenise the column names
  colnames(input_data) = c('area','value')
  ## Match EUMAP ADM0 names
  input_data = mutate(input_data, area = if_else(area == 'GR', 'EL', area)) %>%
    mutate(input_data, area = if_else(area == 'GB', 'UK', area))
  
  jj = left_join(eumap_adm0, input_data, by = c('id' = 'area'))

  g = ggplot(jj, aes(x = long, y = lat, group = group,
                     fill = value)) +
    geom_polygon() +
    geom_path(color = 'gray50', size = 0.25) +
    coord_quickmap(ylim = c(35, 65), xlim = c(-15, 25)) +
    xlab('Longitude') + ylab('Latitude')
  return(g)
}
