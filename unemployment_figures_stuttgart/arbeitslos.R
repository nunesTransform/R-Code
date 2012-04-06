# Bibliotheken
library(ggplot2)
library(reshape)

# einlesen
arbeitslos <- as.data.frame(read.csv("arbeitslose_stgt.csv", sep = ";"))

# benennen
names(arbeitslos) <- c("Jahr", "Gesamt", "<1", "1-3", "3-6", "6-12", "12-24", "<24")

# jahr 2005 rausnehmen, wegen Arbeitsmarktreform
arbeitslos <- subset(arbeitslos, arbeitslos$Jahr!=2005)
arbeitslos <- subset(arbeitslos, arbeitslos$Jahr!=2006)

# transformieren
m.ar <- melt(arbeitslos, id="Jahr")
m.ar$Jahr <- as.factor(m.ar$Jahr)

# subsets
m.ge <- subset(m.ar, m.ar$variable=="Gesamt")
m.sub <- subset(m.ar, m.ar$variable!="Gesamt")

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
#   bars
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
#   histogramm absolut
###
p <- ggplot(m.sub) + 
  geom_area(aes(x = as.numeric(Jahr) + 1981, y = value, fill = factor(variable)), stat = "identity") +
  xlim(c(1981, 2011)) + xlab("") + ylab("") + 
  scale_fill_brewer("Dauer", palette = "Paired")
svg("area.svg")
p
dev.off()
###
#
# Anteile ausrechnen
#
###
gesamt <- apply(arbeitslos[, 3:8], 1, sum) # Gesamtzahl in Statistik stimmt nicht mit Summe überein
arbeitslos[, 3:8] <- apply(arbeitslos[, 3:8], 2, function(x) x / gesamt)

# transformieren
m.ar <- melt(arbeitslos, id="Jahr")
m.ar$Jahr <- as.factor(m.ar$Jahr)

# subsets
m.ge <- subset(m.ar, m.ar$variable=="Gesamt")
m.sub <- subset(m.ar, m.ar$variable!="Gesamt")


###
#   histogramm anteil
###
p <- ggplot(m.sub) + 
  geom_histogram(aes(x = as.numeric(Jahr) + 1981, y = value, fill = factor(variable)), stat = "identity") +
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