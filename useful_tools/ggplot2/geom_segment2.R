#################################################################################
#
# Source: http://spatialanalysis.co.uk/2012/02/great-maps-ggplot2/
#
# description: change the standard line ends in geom_segment from “butt” to 
# “round” in order that the lines appeared continuous and not with “cracks” in
#
#################################################################################
library('devtools')
dev_mode()
library(ggplot2)
library(proto)

#################################################################################
#
# Create GeomSegment2 function
#
#################################################################################
GeomSegment2 <- proto(ggplot2:::GeomSegment, {
  objname <- "geom_segment2"
  draw <- function(., data, scales, coordinates, arrow=NULL, ...) {
    if (is.linear(coordinates)) {
      return(with(coord_transform(coordinates, data, scales),
                  segmentsGrob(x, y, xend, yend, default.units = "native",
                               gp = gpar(col = alpha(colour, alpha), 
                                         lwd = size * .pt,
                                         lty = linetype, lineend = "round"),
                               arrow = arrow)))
    }
  }
})

geom_segment2 <- function(mapping = NULL, data = NULL, stat = "identity", 
                          position = "identity", arrow = NULL, ...) {
  GeomSegment2$new(mapping = mapping, data = data, stat = stat, 
                   position = position, arrow = arrow, ...)
}