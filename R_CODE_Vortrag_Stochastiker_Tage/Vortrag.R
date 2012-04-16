################################################################################
#
# Von: Simon Müller
# Datum: 01.03.2012
#
################################################################################

# Grafikparameter
source ("../ggplot2_themes/theme_border.R")
theme_set (theme_bw())
opt <- opts (panel.grid.major = theme_blank(), 
             panel.grid.minor = theme_blank(),
             panel.border = theme_border(c("left","bottom")),           
             legend.position = c(0.75, 0.85),
             legend.key = theme_blank(),
             legend.text = theme_text(size = 20),
             legend.title = theme_blank(),
             axis.title.y = theme_text(size = 12, angle=90), 
             axis.title.x = theme_text(size = 12, vjust=0),
             axis.text.x = theme_text(size = 16),
             axis.text.y= theme_text(size = 16))

################################################################################
#
# Benötigte R-Pakete
#
################################################################################
vss <- function() {
  # Lade die benötigten Pakete
  library(nfda)
  library(ggplot2)
  library(grid)
  library(reshape)
}
  

################################################################################
#
# Vorhersage
#
################################################################################

pred.ts <- function(X, sm="pca", smp) {
  # Organisation der Daten
  n <- nrow(X)
  m <- ncol(X)
  learning <- 1:(n-1)
  testing <- n
  X.learn <- X[learning, ]   
  X.testing <- X[testing, ] 
  
  pred <- rep(NA, m)
  for(s in 1:m) {
    X.futur.s <- c()
    X.futur.s <- X[2:n, s]
    X.mod <- FuNopaRe(X.learn, X.futur.s, sm, smp, bandwidth="kNNlCV")
    X.pred <- predict(X.mod, X.testing)
    pred[s] <- X.pred$Prediction
  }
  
  pred
}

################################################################################
################################################################################
##
## Beispiel 1: Sea Surface Temperature
##
################################################################################
################################################################################


################################################################################
#
# Lade die Daten
#
################################################################################
load.SST <- function() {   
  # laden der Daten:
  # Quelle: http://www.cpc.ncep.noaa.gov/data/indices/ 
  # unter Sea Surface Temperature (SST) 0-10 south and 90-80 West monthly
  SST <- read.table ("sstoi.indices.txt", header = TRUE)
  
  # lösche 2012
  SST[-745, ]
}

################################################################################
#
# plot der gesamten Daten als Zeitreihe
#
################################################################################
SST.plotall <- function() {
  # plotten der gesamten Daten bis Ende 2011
  SST$id <- SST$YR + SST$MON / 12
  ggplot(SST) + geom_line(aes(x = id, y = NINO1.2), colour = "royalblue") + 
    opt + xlab("") + ylab("") +
    scale_y_continuous (breaks = c(19:29))
# + xlab ("Year") + ylab ("Temperature in [°C]") +
}  

################################################################################
#
# plot der gesamten Daten bzgl. Jahr
#
################################################################################
SST.plotbyyear <- function() {
  # 1950-2011
  Curves <- matrix (SST$NINO1.2, 12, 62)
  rownames(Curves) <- 1:12 
  colnames(Curves) <- 1950:2011
  Temp.df <- melt(Curves)
  names(Temp.df) <- c("Monat", "Jahr", "Temperatur")
  
  ggplot(Temp.df) + 
    geom_line(aes(x=Monat, y=Temperatur, group=Jahr), colour="royalblue", alpha=0.5) + 
    opt + xlab("") + ylab("") + scale_y_continuous(breaks=c(19:29)) +
    scale_x_continuous(breaks=c(1:12))
}

################################################################################
#
# Vorhersage
#
################################################################################
pred.sst <- function() {
  SST <- load.SST()
  Curves <- t(matrix(SST$NINO1.2, 12, 62))
  Cur.2011 <- Curves[62, ]
  Curves <- Curves[-c(48, 49, 62), ]
  
  smp <- c()
  smp$q <- 2
  smp$range.grid <- c(1, 12)
  smp$nknot <- 3
  
  sm <- "PCA"
  p <- pred.ts(Curves, sm, smp)
  mean((p-Cur.2011)^2)
  
  #plot prediction
  Pred.df <- data.frame(t=rep(1:12, 2),
                        values=c(p, Cur.2011),
                        id=rep(c("Prediction", "True"), each=12))
    
  # Grafik
  ggplot(Pred.df) + 
    geom_line(aes(x=t, y=values, colour=id), lty=1, size=1) +
    scale_colour_manual (values = c("maroon", "royalblue")) +
    geom_point(aes(x=t, y=values, colour=id, shape=id), size=5) +
    opt + xlab("") + ylab("") + scale_y_continuous(breaks=c(19:29)) +
    scale_x_continuous(breaks=c(1:12))
}