#################################################################################
#
# author: Simon Müller
# date: 16.4.2012
# Plot all building of stuttgart
#
#################################################################################
library(osmar)
library(ggplot2)
library(gpclib)
source("conv.R")
theme_set(theme_bw())

# Get Data
#url <- "http://www.overpass-api.de/api/xapi?way[bbox=8.62,49.85,8.68,49.89][highway=*][@meta]"
#url <- "http://www.overpass-api.de/api/xapi?way[bbox=7.30,47.31,10.30,49.50][boundary=administrative][admin_level=8][@meta]"
url <- "http://www.overpass-api.de/api/xapi?way[bbox=9.075,48.70,9.275,48.84][building=*][@meta]"
response <- getURL(url, .encoding = "UTF-8")

# Parse Data
resp <- xmlParse(response)

# Transform paresed data to osmar object
stgt_build <- as_osmar(resp)

save(build_stgt, file="stgt_buildings_osmar.RData")


# convert to shapefile
built <- as_sp(build_stgt, what="")
gpclibPermit()
pbuilt <- c(geom_polygon(data = built, aes(x = long, y = lat, group = group), 
                         colour = "#4B4B4B", fill = "#4F4F4F", lwd = 0.2))

#This step removes the axes labels etc when called in the plot.
xquiet <- scale_x_continuous("", breaks = NA)
yquiet <- scale_y_continuous("", breaks = NA)
quiet <- list(xquiet, yquiet)

# plot
ggplot(building_stgt, aes(x=slon,y=slat)) + pbuilt + 
  scale_size(range = c(0.06, 1.8)) + quiet + 
  scale_colour_gradient(low = "#FFFFFF", high = "#FFFF33", space = "rgb") +  
  opts(panel.background = theme_rect(fill = "#F5F4E0"))