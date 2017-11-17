#' Plot a gridded field using discrete scale
#'
#' This function tries to produce high-quality maps of gridded fields using a discrete color-scale. The function is based on \code{ggplot2}.
#' @param x The field to be plotted, it can be both a matrix or a data frame. See Details for more information.
#' @param lon Vector of longitude values
#' @param lat Vector of latitude values
#' @param lonlim A vector of two values defining the longitude limits for the plot. The default is 'auto': it tries to detect the extreme directly from the provided longitude values.
#' @param òatlim A vector of two values defining the latitude limits for the plot. The default is 'auto': it tries to detect the extreme directly from the provided latitude values.
#' @details Details
#' About x.
#' @export
plot_field_discrete = function(x, lon, lat, lonlim = 'auto', latlim = 'auto', labels = c(), breaks = c(),
                              cscale = "Spectral", rev_cscale = F,
                              show_lat_lon_labels = T,
                               varname = "x", title = c(), mask = NULL, siglev = NULL, plot_only_sig = F, smooth = F, smooth_factor = 5,
                               smooth_theta = 0.5, lineWidth = 0.5, dotSize = 0.5, GRID_STEP = 10, FONT_SIZE = 18, show_legend = T) {

  ## X must be lon [rows] x lat [columns]

  ## Checking longitude range
  if (any(lon > 180)) {
    # convert lon from 0-360 to -180,180
    lon[lon > 180] = lon[lon > 180] - 360
    warning('Transforming longitude from 0,360 to -180,180')
  }

  if (is.array(x) && length(dim(x)) > 2) {
    stop("you cannot plot an array with more than two dimensions")
  }
  #######################
  if (is.character(lonlim)) {
    lonlim = range(lon)
  }
  if (is.character(latlim)) {
    latlim = range(lat)
  }
  ## Load world border shapefile: high-res for 'small' fileds
  if ((diff(lonlim) * diff(latlim)) < 3200) {
    load(system.file("borders", "TM_WORLD_BORDERS-0.3.shp.Rdata", package = "eneaR"))
  } else {
    load(system.file("borders", "TM_WORLD_BORDERS_SIMPL-0.3.shp.Rdata", package = "eneaR"))
  }
  ####################### SMOOTHING PART #########################
  if (smooth && is.vector(x)) {
    warning("Smoothing procedure on vector is not implemented")
    smooth = F
  }

  # In case of matrix, check dimensions
  if (!is.vector(x)) {
    # Check dimensions
    if (dim(x)[1] != length(lon)) {
      if (dim(x)[2] == length(lon)) {
        warning("Latitude and longitude vectors look swapped, data field will be transposed")
        x = t(x)
      } else {
        stop("Dimensions of x are not consistent with the lat-lon vectors provided")
      }
    }
  }
  # Smoothing part
  if (smooth) {
    library(fields)
    library(akima)

    z = image.smooth(x, theta = smooth_theta)  #theta is the bandwidth parameter
    z = interp.surface.grid(list(x = lon, y = lat, z = z$z),
                             grid.list = list(x = seq(lonlim[1], lonlim[2], abs(mean(diff(lon)))/smooth_factor),
                                              y = seq(latlim[1], latlim[2], abs(mean(diff(lat)))/smooth_factor)))
    x = z$z
    lon = z$x
    lat = z$y
  }
  if (!is.vector(x)) {
    # convert to data frame
    dd = melt(x)
    dd[, 1] = lon[dd[, 1]]
    dd[, 2] = lat[dd[, 2]]
    names(dd) = c('lon', 'lat', 'x')

  } else {
    if ((length(x) != length(lon)) || (length(x) != length(lat))) {
      stop('The vector length is not consistent with latitude/longitude length')
    }
    dd = data.frame(lon = lon, lat = lat, x = x)
  }

  dd$orig_x = dd$x

  # Adding default breaks (5 bins)
  if (length(breaks) == 0) {
    breaks = hist(x, plot = F, breaks = 5)$breaks
  }

  # Adding default labels
  if (length(labels) == 0) {
    labels = pretty_labels(breaks)
  }

  dd$x = cut(dd$x, breaks = c(-Inf, breaks, Inf), labels = labels)

  ### SUBSETTING DATA
  if (length(lonlim) > 0) {
    dd = subset(dd, lon >= lonlim[1] & lon <= lonlim[2])
    wmap = subset(wmap, long >= lonlim[1] & long <= lonlim[2])
  }
  if (length(latlim) > 0) {
    dd = subset(dd, lat >= latlim[1] & lat <= latlim[2])
    wmap = subset(wmap, lat >= latlim[1] & lat <= latlim[2])
  }


  ## SIG POINTS
  if (!is.null(mask)) {
    # Longitudes and Latitudes of dots
    mask = mask > siglev
    # If all the points are not significant (i.e. > siglev)
    # skip the computation
    if (any(!mask)) {
      ij_dots = which(mask == FALSE, arr.ind = T)
      lon_dots_all = lon[ij_dots[, 2]]
      lat_dots_all = lat[ij_dots[, 1]]
      int_dots = 1

      lon_dots = lon_dots_all[seq(1, length(lon_dots_all), by = int_dots)]
      lat_dots = lat_dots_all[seq(1, length(lat_dots_all), by = int_dots)]

      ij_data <- data.frame(lon_dots, lat_dots)

      ## MERGE with DD
      names(ij_data) = c("lon", "lat")
      ij_data$sig = 1

      dd = left_join(dd, ij_data)
    } else {
      warning('No points are significant according the specified significance level')
      mask = NULL
    }
  }

  # NORMAL PLOT
  if (!plot_only_sig) {
    g = ggplot() + geom_tile(data = dd, aes(x = lon, y = lat, fill = x, color = x), alpha = 0.8)
    if (!is.null(mask)) {
      g = g + geom_point(data = ij_data, aes(lon_dots, lat_dots), size = dotSize, alpha = 0.75, stroke = 0,
                         shape = 16)  #this line is due to ggplot2.0.0
    }
  } else {
    g = ggplot() + geom_tile(data = filter(dd, sig == 1), aes(x = lon, y = lat, fill = x, color = x), alpha = 0.8)
  }

  g = g + geom_path(data = wmap, aes(x = long, y = lat, group = group), size = lineWidth)

  if (length(cscale) == 1) {
    cscale = brewer.pal(length(labels), cscale)
  } else {
    if (length(cscale) != length(labels)) {
      warning('The palette length is different from the number of breaks. A default palette will be used.')
      cscale = brewer.pal(length(labels), 'Spectral')
    }
  }
  # Reverse color scale
  if (rev_cscale) {
    cscale = rev(cscale)
  }
  g = g + scale_fill_manual(name = varname, values = cscale, drop = F)
  g = g + scale_color_manual(name = varname, values = cscale, drop = F)
  if (!show_legend) {
    g = g + guides(fill = F, color = F)
  }
  g = g + ggtitle(title)
  g = g + theme_bw()  #+ theme(, panel.grid.major = element_line(size=1))
  g = g + scale_x_continuous(expand = c(0, 0), breaks = seq(-180, 180, by = GRID_STEP))
  g = g + scale_y_continuous(expand = c(0, 0), breaks = seq(-180, 180, by = GRID_STEP))
  g = g + coord_cartesian(xlim = lonlim, ylim = latlim)
  g = g + theme(text = element_text(size = FONT_SIZE), panel.border = element_rect(colour = "black", size = 2,
                                                                                   fill = NA), panel.grid.major = element_line(colour = "black", size = 0.25))
  if (show_lat_lon_labels) {
    g = g + xlab("Longitude") + ylab("Latitude")
  } else {
    g = g + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
  }
  return(g)
}
