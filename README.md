panas
======
The goal of this package is to reduce the phriction when working with maps &amp; time-series.

It provides two main functionalities: the visualisation of gridded data & choropleths, and the aggregation from gridded data to time-series using geographical borders. 

# Examples

```
> data(ncep)
> g = plot_field_discrete(z, lon, lat, latlim = c(-30, 70), breaks = c(5e-5, 1e-4), color_scale = 'PuBu', varname = 'prec.', grid_step = 60)
> print(g + + coord_equal())
![alt text](https://github.com/matteodefelice/panas/figures/example_plot1 "Logo Title Text 1")
