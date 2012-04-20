#################################################################################
#
# author: Simon Müller
# date: 19.4.2012
#
# Military expenditure (% of GDP)
# data source: http://data.un.org/Data.aspx?d=WDI&f=Indicator_Code%3aMS.MIL.XPND.GD.ZS
#
#################################################################################

# libraries
library(ggplot2)
theme_set(theme_bw())

# sort data
me$Country.or.Area <- ordered(me$Country.or.Area, 
                              levels = me$Country.or.Area[order(me$Value)])
me <- me[order(me$Value, decreasing = TRUE), ]

# get europe data 
europe <- c("Germany", "France", "Greece", "Spain", "Portugal", 
            "Poland", "Greece", "Sweden", "Switzerland", "United Kingdom", 
            "Turkey", "Finland", "Hungary", "Italy", "Norway", "Ukraine")

# centre data
mp <- me[which(me$Country.or.Area %in% europe), ]
mp$Value <- mp$Value - mean(mp$Value)
mp$col <- factor(sign(mp$Value))

# barplot
ggplot(mp) + 
  geom_bar(aes(x = Country.or.Area, y = Value, fill = col)) + 
  coord_flip() +
  scale_fill_manual (values = c("steelblue", "maroon"), legend=FALSE) +
  ylab(" ") + xlab(" ")