#' Create pretty labels useful to visualise discrete intervals
#'
#' The function creates pretty labels for the breaks used in discrete scales in \code{ggplot2}
#' @author Matteo De Felice
#' @param breaks Vector of breaks' values
#' @param separator_character Character that is used to separate the values of the range shown in the legend. Default is ",".
#' @return A caharacter vector with all the labels. The labels' lenght is equals to the length of the provided breaks +2, the extremes.
pretty_labels <- function(breaks, separator_character = ',') {
  labels = rep(NA, length(breaks)+1)
  labels[1] = paste0('<', breaks[1])
  for (i in 1:(length(breaks)-1)) {
    labels[i+1] = paste0(breaks[i], separator_character, breaks[i+1])
  }
  labels[length(breaks)+1] = paste0('>', breaks[length(breaks)])
  return(labels)
}
