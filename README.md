panas
======
The goal of this package is to reduce the phriction when working with maps &amp; time-series.

It provides two main functionalities: the visualisation of gridded data & choropleths, and the aggregation from gridded data to time-series using geographical borders. 

The package is not available on CRAN, you can get the development version from github using [devtools](https://github.com/hadley/devtools):

``` 
devtools::install_github("matteodefelice/panas")
```

Load the `panas` package:
``` r
library(panas)
```

# Visualisation
This package gives you the possibility to visualise gridded data with the function `plot_field_discrete` and choropleths using NUTS classification (this means only Europe). 

### Gridded data
```
> data(ncep)
> g = plot_field_discrete(z, lon, lat, latlim = c(-30, 70), breaks = c(5e-5, 1e-4), color_scale = 'PuBu', varname = 'prec.', grid_step = 60)
> print(g + coord_equal())
```
![alt text](https://github.com/matteodefelice/panas/blob/master/figures/example_plot1.png "Logo Title Text 1")

### Choropleths
```
> my_data = tibble(area = c('IT', 'ITC', 'UKG', 'ITC1'), value = c('a','b','b', 'c'))
> g = get_european_choropleth(my_data)
> print(g + coord_map('lambert', 35, 58, ylim = c(35, 68), xlim = c(-15, 25)))
```
![alt text](https://github.com/matteodefelice/panas/blob/master/figures/example_plot2.png "Logo Title Text 1")

