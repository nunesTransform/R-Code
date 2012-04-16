#################################################################################
#
# author: Simon Müller
# date: 15.4.2012
# Plot all tram and subway of stuttgart
#
#################################################################################

library(osmar)
library(ggplot2)
library(gpclib)
source("conv.R")
theme_set(theme_bw())

url <- "http://www.overpass-api.de/api/xapi?way[bbox=9.075,48.70,9.275,48.84][railway=*][@meta]"
response <- getURL(url, .encoding = "UTF-8")

# Parse Data
resp <- xmlParse(response)

# Transform paresed data to osmar object
stgt_full <- as_osmar(resp)

# for orientation streets are plotted
build_id <- find(stgt_full, way(tags(k=="railway" & v == "subway")))
build_id <- c(build_id, find(stgt_full, way(tags(k=="railway" & v == "subway_entrance"))))
build_id <- c(build_id, find(stgt_full, way(tags(k=="railway" & v == "tram"))))
build_id <- c(build_id, find(stgt_full, way(tags(k=="railway" & v == "tram_stop"))))

build_ids <- find_down(stgt_full, ids=way(build_id))
build_stgt <- subset(stgt_full, ids=build_ids)
plot_ways(build_stgt, col=gray(0.6))
