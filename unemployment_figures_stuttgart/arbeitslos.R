#################################################################################
#
# author: Simon Müller
# date: 5.4.2012
#
# describtion: i used the unemployment time series from stuttgart to show the 
# power and beauty of ggplot2  
#
#################################################################################

# load libraries
library(ggplot2)
library(reshape2)


#################################################################################
#
# Loading and preparation of the data
#
#################################################################################

# read data in; data cannot be published 
arbeitslos <- as.data.frame(read.csv("arbeitslose_stgt.csv", sep = ";"))

# new names
names(arbeitslos) <- c("Jahr", "Gesamt", "<1", "1-3", "3-6", "6-12", "12-24", "<24")

# clean data; remove data from year jahr 2005 because of the labour market reform
arbeitslos <- subset(arbeitslos, arbeitslos$Jahr!=2005)
arbeitslos <- subset(arbeitslos, arbeitslos$Jahr!=2006)

# transformation of the data
m.ar <- melt(arbeitslos, id="Jahr")
m.ar$Jahr <- as.factor(m.ar$Jahr)

# build subsets
m.ge <- subset(m.ar, m.ar$variable=="Gesamt")
m.sub <- subset(m.ar, m.ar$variable!="Gesamt")


#################################################################################
#
# Plotting the data: heatmap, barplot, histogram
#
#################################################################################

###
#   heatmap
###
p <- ggplot(m.sub, aes(variable, Jahr)) + opts(legend.position = "none") + 
  geom_tile(aes(fill = value), colour = "white") + 
  scale_fill_gradient("Werte", low = "white", high = "steelblue") + 
  xlab("Dauer der Arbeitslosigkeit")
p
svg("heatmap.svg")
dev.off()

###
#   barplot
###
p <- ggplot(m.sub) +
  geom_bar(aes(x = as.numeric(Jahr) + 1981, y = value, 
               fill = factor(variable)), stat = "identity") + 
                 xlab("") + ylab("") + 
                 scale_fill_brewer("", palette = "Paired")
svg("bar.svg")
p
dev.off()

###
#   histogram absolut
###
p <- ggplot(m.sub) + 
  geom_area(aes(x = as.numeric(Jahr) + 1981, y = value, 
                fill = factor(variable)), stat = "identity") +
  xlim(c(1981, 2011)) + xlab("") + ylab("") + 
  scale_fill_brewer("Dauer", palette = "Paired")
svg("area.svg")
p
dev.off()

#################################################################################
#
# Precalculations for graphics with relative values
#
#################################################################################

# Sum in statistics doesn't match with the sum; bad quality
gesamt <- apply(arbeitslos[, 3:8], 1, sum) 
arbeitslos[, 3:8] <- apply(arbeitslos[, 3:8], 2, function(x) x / gesamt)

# transformation
m.ar <- melt(arbeitslos, id="Jahr")
m.ar$Jahr <- as.factor(m.ar$Jahr)

# subsets
m.ge <- subset(m.ar, m.ar$variable=="Gesamt")
m.sub <- subset(m.ar, m.ar$variable!="Gesamt")


#################################################################################
#
# Plotting the data: histogram, pie charts
#
#################################################################################

###
#   histogramm relative
###
p <- ggplot(m.sub) + 
  geom_histogram(aes(x = as.numeric(Jahr) + 1981, y = value, 
                     fill = factor(variable)), stat = "identity") +
  xlim(c(1981, 2011)) + xlab("Jahr") + ylab("Anteil Arbeitslose") + 
  scale_fill_brewer("Dauer", palette = "Paired")
svg("anteil_hist.svg")
p
dev.off()

###
#   pie chart
###
p <- ggplot(m.sub) +
  geom_bar(aes(x = factor(1), y = value, 
               fill = factor(variable)), stat = "identity", width = 1) + 
                 xlab("") + ylab("") + 
                 scale_fill_brewer("", palette = "Paired")
p <- p + coord_polar(theta="y") + facet_wrap(~Jahr) + scale_y_continuous('')
svg("pie.svg")
p
dev.off()