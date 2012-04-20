#################################################################################
#
# author: Simon Müller
# date: 19.4.2012
# describtion: transforming spatilLines object to a data frame
# for plotting it with geom_segments ggplot2
#
#################################################################################

conv_sp_lines_to_seg <- function(spLines) {
  library(plyr)
  extract <- data.frame(do.call(rbind, 
                                llply(unlist(coordinates(spLines), 
                                             recursive = F), unlist)))
  names(extract) <- c("slon", "slat")
  n <- length(extract$slon)
  tmplon <- extract$slon[n]
  tmplat <- extract$slat[n]
  extract <- extract[-n, ]
  extract$elon <- c(extract$slon[-1], tmplon)
  extract$elat <- c(extract$slat[-1], tmplat)
  
  length <- do.call(rbind, 
                    llply(unlist(coordinates(spLines), 
                                 recursive = F), nrow))
  length <- cumsum(length)
  length <- length[-length(length)]
  extract[-length, ] 
}