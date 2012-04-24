#################################################################################
#
# author: Simon Müller
# date: 16.4.2012
# Plot streets from a map
#
# 1. Get osm data:
# osmosis-0.40.1/bin/osmosis --rx baden-wuerttemberg.osm --tf accept-ways highway=motowrway,motorway_link,trunk,primary,primary_link,secondary,secondary_link,tertiary,tertiary_link,living_street,residential --used-node --wx wege_bw.osm
# 2. Transform this to a shape file: e.g. by QGIS
# 3. Transform data to segment data
# 4. Plot data
#
# Plots streets of the downloaded area
#
#################################################################################

library(ggplot2)
library(gpclib)

#if your map data is a shapefile use maptools
library(maptools)
gpclibPermit()


bw_st <- readShapeLines("bw_st.shp")

#convert sp lines to data frame
lines <- conv_sp_lines_to_seg(bw_st)

# load geom_segment2 function
source("../ggplot2/geom_segment2.R")
phighway <- c(geom_segment2(data = lines, aes(xend = elon, yend = elat)))

# This step removes the axes labels etc when called in the plot.
xquiet <- scale_x_continuous("", breaks = NA)
yquiet <- scale_y_continuous("", breaks = NA)
quiet <- list(xquiet, yquiet)

png("bw.png", width=2000, height=2000)
ggplot(lines, aes(x=slon,y=slat)) + phighway + 
  scale_size(range = c(0.06, 1.8)) + quiet + 
  scale_colour_gradient(low = "#FFFFFF", high = "#FFFF33", space = "rgb") +  
  opts(panel.background = theme_rect(fill = "#F5F4E0"))
dev.off()