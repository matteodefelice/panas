#' Prepare a ggplot2-based choropleth on European national (NUTS0) countries.
#' @param input_data A data frame containing the data to be mapped. It should contain two columns: the first containing the ISO ALPHA-2
#' code of the country, the second the value that should be mapped
#' @return A ggplot2 object
#' @export
#'
get_european_choropleth <- function(input_data) {
  library(dplyr)
  library(ggplot2)
  #### Load shapefiles
  load('~/Documents/Work/ecemathon/chorofunction/eumap_adm0.rda')
  #### Check the input data frame
  if (!is.data.frame(input_data)) {
    stop('The input_data is not a data frame')
  } else if (ncol(input_data)!=2) {
    stop('Input data frame has the wrong number of columns (!=2) ')
  }
  #### Homogenise the column names
  colnames(input_data) = c('area','value')
  ## Match EUMAP ADM0 names
  input_data = mutate(input_data, area = if_else(area == 'GR', 'EL', area))
  jj = left_join(eumap_adm0, input_data, by = c('id' = 'area'))

  g = ggplot(jj, aes(x = long, y = lat, group = group,
                     fill = value)) +
    geom_polygon() +
    geom_path(color = 'gray50', size = 0.25) +
    # scale_fill_brewer(palette = "PiYG", na.value = 'lightgrey', name = 'Corr.', drop = F) +

    coord_quickmap(ylim = c(35, 65), xlim = c(-15, 25)) +
    xlab('Longitude') + ylab('Latitude')
  return(g)
}
