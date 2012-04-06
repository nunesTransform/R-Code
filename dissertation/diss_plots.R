#################################################################################
#
# author: Simon Müller
#
# need: 
# - theme_border.R, viewport.R (can be found in: useful_tools/ggplot2)
# - get_data.R 
#
# libraries:
# - nfda, ggplot2, tikz, fds
#
#################################################################################

burba.diss <-function(n = 1000, sigma = 1) {
  
  smp.name <- paste("SMP_Burba_n1000_Sigma", sigma, ".tex" ,sep="")
  cur.name <- paste("Curve_Burba_n1000_Sigma", sigma, ".tex" ,sep="")
  exBoot.name <- paste("Example_Boot_Burba_n1000_Sigma", sigma, ".tex" ,sep="")
  exCV.name <- paste("Example_CV_Burba_n1000_Sigma", sigma, ".tex" ,sep="")
  sigma <- .01
  Data.df <- load.fds.sim (dataset = "Burbaetal", n = n, sigma = sigma)
  Curves <- Data.df$X
  Y <- Data.df$Y
  
  # aufteilen in lern- und testdaten
  learning <- sample (1:n, floor (n / 2))
  testing <- (1:n)[-learning] 
  CurvesL <- Curves[learning, ]
  CurvesT <- Curves[testing, ]
  YL <- Y[learning]
  YT <- Y[testing]  
  
  # vorhersage mittels bootstrap-verfahren / globalem CV
  Pred <- Kernel.Regression ( YL, 
                              CurvesL, CurvesT, 
                              range.grid = c(0, 2 * pi), 
                              q = 0, 
                              nknot = 20, 
                              semimetric = "deriv", 
                              kernel.func = "quadratic", 
                              Resampling.Method = "homoscedatic", 
                              NB = 100, 
                              Bandwidth.Neighbours = 20, 
                              method = "globalbootstrap")
  Pred.Boot <- Pred$Bootstrap.Predicted.values
  Pred.CV <- Pred$CV.Predicted.Values
  
  Mse.Boot <- round (mean ((Pred.Boot - YT)^2), 4)
  Mse.CV <- round (mean ((Pred.CV - YT)^2), 4)
  
###########
##
##  Plot Prediction
##
###########
  #plotte die vorhersagen: CV
  pred.df <- data.frame (id = "Kreuzvalidierung",
                         pred = Pred.CV,
                         Y = YT,
                         col = (testing > 500))
  tikz(exCV.name, width = 2.75, height = 2.75)
  ggplot (pred.df) + 
    geom_point (aes (x = pred, y = Y, col = col), size = 1, alpha = .8) + 
    scale_shape (solid = FALSE) + 
    scale_colour_manual (values = c("royalblue", "maroon"), legend=FALSE) +
     geom_line (aes (y = Y, x = Y)) + ylab ("$Y_i$") + xlab ("$\\hat{m}_g(X_i)$") + theme_bw() + 
     opts(axis.title.y = theme_text(size = 8, angle=0), 
          axis.title.x = theme_text(size = 8, vjust=0),
          axis.text.x = theme_text(size = 6),
          axis.text.y= theme_text(size = 6)) 
  dev.off()

  
  #bootstrapping
  pred.df <- data.frame (id = "Bootstrapping",
                         pred = Pred.Boot,
                         Y = YT,
                         col = (testing > 500))
  tikz(exBoot.name, width = 2.75, height = 2.75)
  ggplot (pred.df) + 
    geom_point (aes (x = pred, y = Y, col = col), size = 1, alpha = .8) + 
    scale_shape (solid = FALSE) + 
    scale_colour_manual (values = c("royalblue", "maroon"), legend=FALSE) +
     geom_line (aes (y = Y, x = Y)) + ylab ("$Y_i$") + xlab ("$\\hat{m}_{gh}^{*}(X_i)$") + theme_bw() + 
     opts(axis.title.y = theme_text(size = 8, angle=0), 
          axis.title.x = theme_text(size = 8, vjust=0),
          axis.text.x = theme_text(size = 6),
          axis.text.y= theme_text(size = 6)) 
  dev.off()

###########
##
##  Plot Curves
##
###########
  x = seq(0, 2 * pi, length = 100)
  xx <- floor(seq(1, 100, length = 50))                   
  df <- data.frame (x = x[xx], 
                    y = as.vector(t(Curves[c(1:100, 501:600), xx])),
                    dd = rep(Y[c(1:100, 501:600)], each = 50),
                    col = as.factor(c(rep(1:2, each = 50 * 100))),
                    Id = rep(1:200, each = 50))
  tikz(cur.name, width = 2.75, height = 2.75)
  ggplot (df) + 
      geom_line (aes(x = x, y = y, group = Id, col = col), alpha = 0.25) + 
       xlab (" ") + ylab (" ") + 
       scale_colour_manual (values = c("royalblue", "maroon"), legend=FALSE) + 
       opts(axis.title.y = theme_text(size = 8, angle=90), 
            axis.title.x = theme_text(size = 8, vjust=0),
            axis.text.x = theme_text(size = 6),
            axis.text.y= theme_text(size = 6))
  dev.off()
  
###########
##
##  ESMP for all data
##
###########
  library(nfda)
  semimetric <- SemimetricDeriv (Curves, 
                                 Curves, 
                                 q = 0, 
                                 nknot = 20, 
                                 range.grid = c(0, 2 * pi))
  semimetric <- semimetric$semimetric
                                
  tikz(smp.name, width = 2.75, height = 2.75)
  vec <- smp(semimetric, Pred$CV.hopt)
  data <- data.frame(x = 1:n, y = vec, col = as.factor(rep(1:2, each = floor(n/2))))

  p1 <- ggplot(data) + geom_point(aes(x = x, y = y, colour = col), alpha = .75, size = 1) + 
    xlab("index") + 
    scale_colour_manual (values = c("royalblue", "maroon"), legend=FALSE) +
    ylab("empirical small ball probability") + ylim(c(0,1)) + 
     opts(axis.title.y = theme_text(size = 8, angle=90), 
          axis.title.x = theme_text(size = 8, vjust=0),
          axis.text.x = theme_text(size = 6),
          axis.text.y= theme_text(size = 6))
  p2 <- ggplot(data) + geom_density(aes(x = y), col = "royalblue") + coord_flip() +
    ylab(" ") + xlab(" ") + xlim(c(0,1))+ 
     opts(axis.title.y = theme_blank(), 
          axis.title.x = theme_text(size = 8),
          axis.text.x = theme_text(size = 8, col = "white"),
          axis.text.y = theme_blank(),
          axis.ticks = theme_blank(), 
          panel.grid.major = theme_blank(), 
          panel.grid.minor = theme_blank(),
          panel.border = theme_border(c("none")))
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 20)))
  print(p1, vp = vplayout(x = 1, y = 1:16))
  print(p2, vp = vplayout(x = 1, y = 14:20))
  dev.off()
}

octane.diss <-function() {
  n <- 60
  Data.df <- load.fds.sim (dataset = "Octane", n = n)
  Curves <- Data.df$X
  Y <- Data.df$Y
  
  # aufteilen in lern- und testdaten
  learning <- sample (1:n, floor (n / 2))
  testing <- (1:n)[-learning] 
  CurvesL <- Curves[learning, ]
  CurvesT <- Curves[testing, ]
  YL <- Y[learning]
  YT <- Y[testing]  
  
  # vorhersage mittels bootstrap-verfahren / globalem CV
  Pred <- Kernel.Regression ( YL, 
                              CurvesL, CurvesT, 
                              range.grid = c(900, 1700), 
                              q = 1, 
                              nknot = 20, 
                              semimetric = "deriv", 
                              kernel.func = "quadratic", 
                              Resampling.Method = "homoscedatic", 
                              NB = 100, 
                              Bandwidth.Neighbours = 20, 
                              method = "globalbootstrap")
  Pred.Boot <- Pred$Bootstrap.Predicted.values
  Pred.CV <- Pred$CV.Predicted.Values
  
  Mse.Boot <- round (mean ((Pred.Boot - YT)^2), 4)
  Mse.CV <- round (mean ((Pred.CV - YT)^2), 4)
  
###########
##
##  Plot Prediction
##
###########
  #plotte die vorhersagen: CV
  pred.df <- data.frame (id = "Kreuzvalidierung",
                         pred = Pred.CV,
                         Y = YT,
                         col = testing > 500)
  tikz("OctCVone.tex", width = 2.75, height = 2.75)
  ggplot (pred.df) + 
    geom_point (aes (x = pred, y = Y, col = col), size = 1, alpha = .8) + 
    scale_shape (solid = FALSE) + 
    scale_colour_manual (values = c("royalblue", "maroon"), legend=FALSE) +
     geom_line (aes (y = Y, x = Y)) + ylab ("$Y_i$") + xlab ("$\\hat{m}_g(X_i)$") + theme_bw() + 
     opts(axis.title.y = theme_text(size = 8, angle=0), 
          axis.title.x = theme_text(size = 8, vjust=0),
          axis.text.x = theme_text(size = 6),
          axis.text.y= theme_text(size = 6)) 
  dev.off()

  
  #bootstrapping
  pred.df <- data.frame (id = "Bootstrapping",
                         pred = Pred.Boot,
                         Y = YT,
                         col = (testing > 500))
  tikz("OctBootone.tex", width = 2.75, height = 2.75)
  ggplot (pred.df) + 
    geom_point (aes (x = pred, y = Y, col = col), size = 1, alpha = .8) + 
    scale_shape (solid = FALSE) + 
    scale_colour_manual (values = c("royalblue", "maroon"), legend=FALSE) +
     geom_line (aes (y = Y, x = Y)) + ylab ("$Y_i$") + xlab ("$\\hat{m}_{gh}^{*}(X_i)$") + theme_bw() + 
     opts(axis.title.y = theme_text(size = 8, angle=0), 
          axis.title.x = theme_text(size = 8, vjust=0),
          axis.text.x = theme_text(size = 6),
          axis.text.y= theme_text(size = 6)) 
  dev.off()

###########
##
##  Plot Curves
##
###########
  xx <- floor(seq(1, 400, length = 100))
  x <- seq(900, 1700, length = 401)
  df <- data.frame (x = x[xx], 
                    y = as.vector(t(Curves[ , xx])),
                    dd = rep(Y, each = 100),
                    col = as.factor(c(rep(2, times = 100 * n))),
                    Id = rep(1:n, each = 100))
  tikz("Octanederv0Curves.tex", width = 2.75, height = 2.75)
  ggplot (df) + 
      geom_line (aes(x = x, y = y, group = Id, col = col), alpha = 0.25) + 
        xlab ("wavelength in $[$nm$]$") + ylab ("absorbtion") + 
       scale_colour_manual (values = c("royalblue"), legend=FALSE) + 
       opts(axis.title.y = theme_text(size = 8, angle=90), 
            axis.title.x = theme_text(size = 8, vjust=0),
            axis.text.x = theme_text(size = 6),
            axis.text.y= theme_text(size = 6))
  dev.off()
  
###########
##
##  ESMP for all data
##
###########
  library(nfda)
  semimetric <- SemimetricDeriv (Curves, 
                                 Curves, 
                                 q = 1, 
                                 nknot = 20, 
                                 range.grid = c(900, 1700))
  semimetric <- semimetric$semimetric
                                
  tikz("SMPOctaneSpectrum.tex", width = 2.75, height = 2.75)
  vec <- smp(semimetric, Pred$CV.hopt)
  data <- data.frame(x = 1:n, y = vec, col = as.factor(rep(1, each = n)))

  p1 <- ggplot(data) + geom_point(aes(x = x, y = y, colour = col), alpha = 1, size = 1) + 
    xlab("index") + 
    scale_colour_manual (values = c("royalblue", "maroon"), legend=FALSE) +
    ylab("empirical small ball probability") + ylim(c(0,1)) + 
     opts(axis.title.y = theme_text(size = 8, angle=90), 
          axis.title.x = theme_text(size = 8, vjust=0),
          axis.text.x = theme_text(size = 6),
          axis.text.y= theme_text(size = 6))
  p2 <- ggplot(data) + geom_density(aes(x = y), col = "royalblue") + coord_flip() +
    ylab(" ") + xlab(" ") + xlim(c(0,1))+ 
     opts(axis.title.y = theme_blank(), 
          axis.title.x = theme_text(size = 8),
          axis.text.x = theme_text(size = 8, col = "white"),
          axis.text.y = theme_blank(),
          axis.ticks = theme_blank(), 
          panel.grid.major = theme_blank(), 
          panel.grid.minor = theme_blank(),
          panel.border = theme_border(c("none")))
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 20)))
  print(p1, vp = vplayout(x = 1, y = 1:16))
  print(p2, vp = vplayout(x = 1, y = 14:20))
  dev.off()
}
 
moisture.diss <-function() {
  n <- 100
  Data.df <- load.fds.sim (dataset = "Moisture", n = n)
  Curves <- Data.df$X
  Y <- Data.df$Y
  
  # aufteilen in lern- und testdaten
  learning <- sample (1:n, floor (n / 2))
  testing <- (1:n)[-learning] 
  CurvesL <- Curves[learning, ]
  CurvesT <- Curves[testing, ]
  YL <- Y[learning]
  YT <- Y[testing]  
  
  # vorhersage mittels bootstrap-verfahren / globalem CV
  Pred <- Kernel.Regression ( YL, 
                              CurvesL, CurvesT, 
                              range.grid = c(1100, 2500), 
                              q = 2, 
                              nknot = 20, 
                              semimetric = "deriv", 
                              kernel.func = "quadratic", 
                              Resampling.Method = "homoscedatic", 
                              NB = 100, 
                              Bandwidth.Neighbours = 20, 
                              method = "globalbootstrap")
  Pred.Boot <- Pred$Bootstrap.Predicted.values
  Pred.CV <- Pred$CV.Predicted.Values
  
  Mse.Boot <- round (mean ((Pred.Boot - YT)^2), 4)
  Mse.CV <- round (mean ((Pred.CV - YT)^2), 4)
  
###########
##
##  Plot Prediction
##
###########
  #plotte die vorhersagen: CV
  pred.df <- data.frame (id = "Kreuzvalidierung",
                         pred = Pred.CV,
                         Y = YT,
                         col = testing > 500)
  tikz("CVMoistureOne.tex", width = 2.75, height = 2.75)
  ggplot (pred.df) + 
    geom_point (aes (x = pred, y = Y, col = col), size = 1, alpha = .8) + 
    scale_shape (solid = FALSE) + 
    scale_colour_manual (values = c("royalblue", "maroon"), legend=FALSE) +
     geom_line (aes (y = Y, x = Y)) + ylab ("$Y_i$") + xlab ("$\\hat{m}_g(X_i)$") + theme_bw() + 
     opts(axis.title.y = theme_text(size = 8, angle=0), 
          axis.title.x = theme_text(size = 8, vjust=0),
          axis.text.x = theme_text(size = 6),
          axis.text.y= theme_text(size = 6)) 
  dev.off()

  
  #bootstrapping
  pred.df <- data.frame (id = "Bootstrapping",
                         pred = Pred.Boot,
                         Y = YT,
                         col = (testing > 500))
  tikz("BootMoistureOne.tex", width = 2.75, height = 2.75)
  ggplot (pred.df) + 
    geom_point (aes (x = pred, y = Y, col = col), size = 1, alpha = .8) + 
    scale_shape (solid = FALSE) + 
    scale_colour_manual (values = c("royalblue", "maroon"), legend=FALSE) +
     geom_line (aes (y = Y, x = Y)) + ylab ("$Y_i$") + xlab ("$\\hat{m}_{gh}^{*}(X_i)$") + theme_bw() + 
     opts(axis.title.y = theme_text(size = 8, angle=0), 
          axis.title.x = theme_text(size = 8, vjust=0),
          axis.text.x = theme_text(size = 6),
          axis.text.y= theme_text(size = 6)) 
  dev.off()

###########
##
##  Plot Curves
##
###########
  
  
  xx <- floor(seq(1, 700, length = 100))
  x = seq(1100, 2500, length = 701)
  df <- data.frame (x = x[xx], 
                    y = as.vector(t(Curves[, xx])),
                    dd = rep(Y, each = 100),
                    col = as.factor(c(rep(2, times = 100 * n))),
                    Id = rep(1:n, each = 100))
  tikz("Moisturederv0Curves.tex", width = 2.75, height = 2.75)
  ggplot (df) + 
      geom_line (aes(x = x, y = y, group = Id, col = col), alpha = 1) + 
        xlab ("wavelength in $[$nm$]$") + ylab ("absorbtion") + 
       scale_colour_manual (values = c("royalblue"), legend=FALSE) + 
       opts(axis.title.y = theme_text(size = 8, angle=90), 
            axis.title.x = theme_text(size = 8, vjust=0),
            axis.text.x = theme_text(size = 6),
            axis.text.y= theme_text(size = 6))
  dev.off()
  
###########
##
##  ESMP for all data
##
###########
  library(nfda)
  semimetric <- SemimetricDeriv (Curves, 
                                 Curves, 
                                 q = 2, 
                                 nknot = 20, 
                                 range.grid = c(1100, 2500))
  semimetric <- semimetric$semimetric
                                
  tikz("SMPMoistureSpectrum.tex", width = 2.75, height = 2.75)
  vec <- smp(semimetric, Pred$CV.hopt)
  data <- data.frame(x = 1:n, y = vec, col = as.factor(rep(1, each = n)))

  p1 <- ggplot(data) + geom_point(aes(x = x, y = y, colour = col), alpha = 1, size = 1) + 
    xlab("index") + 
    scale_colour_manual (values = c("royalblue", "maroon"), legend=FALSE) +
    ylab("empirical small ball probability") + ylim(c(0,1)) + 
     opts(axis.title.y = theme_text(size = 8, angle=90), 
          axis.title.x = theme_text(size = 8, vjust=0),
          axis.text.x = theme_text(size = 6),
          axis.text.y= theme_text(size = 6))
  p2 <- ggplot(data) + geom_density(aes(x = y), col = "royalblue") + coord_flip() +
    ylab(" ") + xlab(" ") + xlim(c(0,1))+ 
     opts(axis.title.y = theme_blank(), 
          axis.title.x = theme_text(size = 8),
          axis.text.x = theme_text(size = 8, col = "white"),
          axis.text.y = theme_blank(),
          axis.ticks = theme_blank(), 
          panel.grid.major = theme_blank(), 
          panel.grid.minor = theme_blank(),
          panel.border = theme_border(c("none")))
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 20)))
  print(p1, vp = vplayout(x = 1, y = 1:16))
  print(p2, vp = vplayout(x = 1, y = 14:20))
  dev.off()
}

fat.diss <-function() {
  n <- 215
  Data.df <- load.fds.sim (dataset = "Fat", n = n)
  Curves <- Data.df$X
  Y <- Data.df$Y
  
  # aufteilen in lern- und testdaten
  learning <- sample (1:n, floor (n / 2))
  testing <- (1:n)[-learning] 
  CurvesL <- Curves[learning, ]
  CurvesT <- Curves[testing, ]
  YL <- Y[learning]
  YT <- Y[testing]  
  
  # vorhersage mittels bootstrap-verfahren / globalem CV
  Pred <- Kernel.Regression ( YL, 
                              CurvesL, CurvesT, 
                              range.grid = c(850, 1050), 
                              q = 2, 
                              nknot = 20, 
                              semimetric = "deriv", 
                              kernel.func = "quadratic", 
                              Resampling.Method = "homoscedatic", 
                              NB = 100, 
                              Bandwidth.Neighbours = 20, 
                              method = "globalbootstrap")
  Pred.Boot <- Pred$Bootstrap.Predicted.values
  Pred.CV <- Pred$CV.Predicted.Values
  
  Mse.Boot <- round (mean ((Pred.Boot - YT)^2), 4)
  Mse.CV <- round (mean ((Pred.CV - YT)^2), 4)
  
###########
##
##  Plot Prediction
##
###########
  #plotte die vorhersagen: CV
  pred.df <- data.frame (id = "Kreuzvalidierung",
                         pred = Pred.CV,
                         Y = YT,
                         col = testing > 500)
  tikz("CVFatOne.tex", width = 2.75, height = 2.75)
  ggplot (pred.df) + 
    geom_point (aes (x = pred, y = Y, col = col), size = 1, alpha = .8) + 
    scale_shape (solid = FALSE) + 
    scale_colour_manual (values = c("royalblue", "maroon"), legend=FALSE) +
     geom_line (aes (y = Y, x = Y)) + ylab ("$Y_i$") + xlab ("$\\hat{m}_g(X_i)$") + theme_bw() + 
     opts(axis.title.y = theme_text(size = 8, angle=0), 
          axis.title.x = theme_text(size = 8, vjust=0),
          axis.text.x = theme_text(size = 6),
          axis.text.y= theme_text(size = 6)) 
  dev.off()

  
  #bootstrapping
  pred.df <- data.frame (id = "Bootstrapping",
                         pred = Pred.Boot,
                         Y = YT,
                         col = (testing > 500))
  tikz("BootFatOne.tex", width = 2.75, height = 2.75)
  ggplot (pred.df) + 
    geom_point (aes (x = pred, y = Y, col = col), size = 1, alpha = .8) + 
    scale_shape (solid = FALSE) + 
    scale_colour_manual (values = c("royalblue", "maroon"), legend=FALSE) +
     geom_line (aes (y = Y, x = Y)) + ylab ("$Y_i$") + xlab ("$\\hat{m}_{gh}^{*}(X_i)$") + theme_bw() + 
     opts(axis.title.y = theme_text(size = 8, angle=0), 
          axis.title.x = theme_text(size = 8, vjust=0),
          axis.text.x = theme_text(size = 6),
          axis.text.y= theme_text(size = 6)) 
  dev.off()

###########
##
##  Plot Curves
##
###########
  sam <- sample(1:n, 50)
  df <- data.frame (x = seq(850, 1050, length = 100), 
                    y = as.vector(t(Curves)),
                    dd = rep(Y, each = 100),
                    col = as.factor(c(rep(2, times = 100 * n))),
                    Id = rep(1:n, each = 100))
  tikz("Fatderv0Curves.tex", width = 2.75, height = 2.75)
  ggplot (df) + 
      geom_line (aes(x = x, y = y, group = Id, col = col), alpha = 1) + 
        xlab ("wavelength in $[$nm$]$") + ylab ("absorbtion") + 
       scale_colour_manual (values = c("royalblue"), legend=FALSE) + 
       opts(axis.title.y = theme_text(size = 8, angle=90), 
            axis.title.x = theme_text(size = 8, vjust=0),
            axis.text.x = theme_text(size = 6),
            axis.text.y= theme_text(size = 6))
  dev.off()
  
###########
##
##  ESMP for all data
##
###########
  library(nfda)
  semimetric <- SemimetricDeriv (Curves, 
                                 Curves, 
                                 q = 2, 
                                 nknot = 20, 
                                 range.grid = c(850, 1050))
  semimetric <- semimetric$semimetric
                                
  tikz("SMPFatSpectrum.tex", width = 2.75, height = 2.75)
  vec <- smp(semimetric, Pred$CV.hopt)
  data <- data.frame(x = 1:n, y = vec, col = as.factor(rep(1, each = n)))

  p1 <- ggplot(data) + geom_point(aes(x = x, y = y, colour = col), alpha = 1, size = 1) + 
    xlab("index") + 
    scale_colour_manual (values = c("royalblue", "maroon"), legend=FALSE) +
    ylab("empirical small ball probability") + ylim(c(0,1)) + 
     opts(axis.title.y = theme_text(size = 8, angle=90), 
          axis.title.x = theme_text(size = 8, vjust=0),
          axis.text.x = theme_text(size = 6),
          axis.text.y= theme_text(size = 6))
  p2 <- ggplot(data) + geom_density(aes(x = y), col = "royalblue") + coord_flip() +
    ylab(" ") + xlab(" ") + xlim(c(0,1))+ 
     opts(axis.title.y = theme_blank(), 
          axis.title.x = theme_text(size = 8),
          axis.text.x = theme_text(size = 8, col = "white"),
          axis.text.y = theme_blank(),
          axis.ticks = theme_blank(), 
          panel.grid.major = theme_blank(), 
          panel.grid.minor = theme_blank(),
          panel.border = theme_border(c("none")))
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 20)))
  print(p1, vp = vplayout(x = 1, y = 1:16))
  print(p2, vp = vplayout(x = 1, y = 14:20))
  dev.off()
}

rw.boxplot <- function() {
  source("~/Dropbox/R Paket/vgl_neu.R")
  source("~/Dropbox/R Paket/models.regression.R")
  require(fds)
  
  ###########
  ##
  ##  FAT DATA SET
  ##
  ###########
  pfat <- fda.testing.sim_cpp(k = 200, vec.length.data = 215, dataset = "Fat")
  tikz("fatboxplot.tex", width = 5.5, height = 2.75)
  p1 <-   pfat$p + xlim("global CV") + 
                  ylab("empirical mean square error") + 
                   opts(axis.title.y = theme_text(size = 8, angle=90), 
                        axis.title.x = theme_text(size = 8, vjust=0),
                        axis.text.x = theme_text(size = 6),
                        axis.text.y = theme_text(size = 6))
  p2 <-   pfat$p + xlim("bootstrapping") + 
                  ylab("empirical mean square error") +
                   opts(axis.title.y = theme_blank(), 
                        axis.title.x = theme_text(size = 8, vjust=0),
                        axis.text.x = theme_text(size = 6),
                        axis.text.y = theme_text(size = 6))          
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 20)))
  print(p1, vp = vplayout(x = 1, y = 1:10))
  print(p2, vp = vplayout(x = 1, y = 11:20))
  dev.off()
  
  ###########
  ##
  ##  MOISTURE DATA SET
  ##
  ###########
  pmoi <- fda.testing.sim_cpp(k = 200, vec.length.data = 100, dataset = "Moisture")
  tikz("moiboxplot.tex", width = 5.5, height = 2.75)
  p1 <-   pmoi$p + xlim("global CV") + 
                  ylab("empirical mean square error") + 
                   opts(axis.title.y = theme_text(size = 8, angle=90), 
                        axis.title.x = theme_text(size = 8, vjust=0),
                        axis.text.x = theme_text(size = 6),
                        axis.text.y = theme_text(size = 6))
  p2 <-   pmoi$p + xlim("bootstrapping") + 
                  ylab("empirical mean square error") +
                   opts(axis.title.y = theme_blank(), 
                        axis.title.x = theme_text(size = 8, vjust=0),
                        axis.text.x = theme_text(size = 6),
                        axis.text.y = theme_text(size = 6))          
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 20)))
  print(p1, vp = vplayout(x = 1, y = 1:10))
  print(p2, vp = vplayout(x = 1, y = 11:20))
  dev.off()
  
  ###########
  ##
  ##  OCTANE DATA SET
  ##
  ###########
  poct <- fda.testing.sim_cpp(k = 200, vec.length.data = 60, dataset = "Octane")
  tikz("octboxplot.tex", width = 5.5, height = 2.75)
  p1 <-   poct$p + xlim("global CV") + 
                  ylab("empirical mean square error") + 
                   opts(axis.title.y = theme_text(size = 8, angle=90), 
                        axis.title.x = theme_text(size = 8, vjust=0),
                        axis.text.x = theme_text(size = 6),
                        axis.text.y = theme_text(size = 6))
  p2 <-   poct$p + xlim("bootstrapping") + 
                  ylab("empirical mean square error") +
                   opts(axis.title.y = theme_blank(), 
                        axis.title.x = theme_text(size = 8, vjust=0),
                        axis.text.x = theme_text(size = 6),
                        axis.text.y = theme_text(size = 6))          
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 20)))
  print(p1, vp = vplayout(x = 1, y = 1:10))
  print(p2, vp = vplayout(x = 1, y = 11:20))
  dev.off()
  wilcox.test(pb$data$fehler[1:200], pb$data$fehler[201:400])
}

burbaetal.boxplot <- function() {
  source("~/Dropbox/R Paket/vgl_neu.R")
  source("~/Dropbox/R Paket/models.regression.R")
  require(nfda)
  
  ###########
  ##
  ##  FAT DATA SET
  ##
  ###########
  pb <- fda.testing.sim_cpp(k = 200, 
                           range.grid = c(0, 2*pi), 
                           vec.length.data = 1000, 
                           dataset = "Burbaetal", 
                           sigma = 1)
  tikz("sigma10boxplota.tex", width = 5.5, height = 2.75)
  p1 <-   pb$p + xlim("global CV") + 
                ylab("empirical mean square error") + 
                 opts(axis.title.y = theme_text(size = 8, angle=90), 
                      axis.title.x = theme_text(size = 8, vjust=0),
                      axis.text.x = theme_text(size = 6),
                      axis.text.y = theme_text(size = 6))
  p2 <-   pb$p + xlim("bootstrapping") + 
                ylab("empirical mean square error") +
                 opts(axis.title.y = theme_blank(), 
                      axis.title.x = theme_text(size = 8, vjust=0),
                      axis.text.x = theme_text(size = 6),
                      axis.text.y = theme_text(size = 6))          
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 20)))
  print(p1, vp = vplayout(x = 1, y = 1:10))
  print(p2, vp = vplayout(x = 1, y = 11:20))
  dev.off()
  wilcox.test(poct$data$fehler[1:200], poct$data$fehler[201:400])
}

smp.plotting <- function(vec.length.data, dataset = "Burbaetal", sigma = .1) {
  data <- load.fds.sim(dataset, vec.length.data, ...)
  Curves <- data$X
  Y <- data$Y
  
  range.grid <- c(0, 2*pi)
  n.test <- floor(0.5 * vec.length.data)
  n.learn <- vec.length.data - n.test
  learn <- 1:n.test 
  test <- (1:vec.length.data)[-learn]
  Curves.Learning <- Curves[learn,] 
  Curves.Testing <- Curves[test,] 
  Y.learn <- Y[learn]
  Y.test <- Y[test]
  
  Pred <- Kernel.Regression(Y.learn, Curves.Learning, Curves.Testing, range.grid = range.grid, q = 0, nknot = 20, semimetric = "deriv", kernel.func = "quadratic", Bandwidth.Neighbours = 20, method = "globalCV")
  Distance.Matrix <- Get.Distance.Matrix(Curves, Curves, range.grid = range.grid, q = 0, nknot = 20, semimetric = "deriv")
  vec <- smp(Distance.Matrix, Pred$CV.hopt)
  data <- data.frame(x = 1:vec.length.data, y = vec)
  p <- ggplot(data, aes(x, y)) + geom_point() + xlab("Index") + ylab("Small Ball Probability") + theme_bw() + ylim(c(0,1))
  return( data ) 
}

  
# calc esmp for sigma = 0.1  
globloc.diss <-function() {
  n <- 1000
  Data.df <- load.fds.sim (dataset = "Burbaetal", n = n, sigma = 0.1)
  Curves <- Data.df$X
  Y <- Data.df$Y
  
  n.test <- floor(0.5 * n)
  n1 <- floor(n.test / 2) 
  n2 <- n.test - n1
  n.learn <- n - n.test
  testing <- c(sample(1:n.test, n1), sample((n.test+1):n, n2))
  learning <- (1:n)[-testing]
  
  # aufteilen in lern- und testdaten
  CurvesL <- Curves[learning, ]
  CurvesT <- Curves[testing, ]
  YL <- Y[learning]
  YT <- Y[testing]  
  
  # vorhersage mittels bootstrap-verfahren / globalem CV
  Pred <- Kernel.Regression ( YL, 
                              CurvesL, CurvesT, 
                              range.grid = c(0, 2 * pi), 
                              q = 0, 
                              nknot = 20, 
                              semimetric = "deriv", 
                              kernel.func = "quadratic", 
                              Resampling.Method = "homoscedatic", 
                              NB = 100, 
                              Bandwidth.Neighbours = 20, 
                              method = "globalbootstrap")
  Pred.Boot <- Pred$Bootstrap.Predicted.values
  Pred.CV <- Pred$CV.Predicted.Values
  
  Mse.Boot <- round (mean ((Pred.Boot - YT)^2), 4)
  Mse.CV <- round (mean ((Pred.CV - YT)^2), 4)
 
###########
##
##  ESMP for test data
##
###########
  library(nfda)
  semimetric <- SemimetricDeriv (CurvesT, 
                                 CurvesT, 
                                 q = 0, 
                                 nknot = 20, 
                                 range.grid = c(0, 2 * pi))
  semimetric <- semimetric$semimetric
  vec <- smp(semimetric, Pred$CV.hopt)

  tikz("SMPTestSetGlobal.tex", width = 2.75, height = 2.75) 
  data <- data.frame(x = 1:n.test, y = vec, col = as.factor(testing > 500))
  ggplot(data) + geom_point(aes(x = x, y = y, colour = col), alpha = 1, size = 1) + 
    xlab("index") + 
    scale_colour_manual (values = c("royalblue", "maroon"), legend=FALSE) +
    ylab("empirical small ball probability") + ylim(c(0,1)) + 
     opts(axis.title.y = theme_text(size = 8, angle=90), 
          axis.title.x = theme_text(size = 8, vjust=0),
          axis.text.x = theme_text(size = 6),
          axis.text.y= theme_text(size = 6))
  dev.off()
  
  vec <- c()
  for (i in 1:n.test) {
    vec[i] <- smp(semimetric[i, ], Pred$Bootstrap.hopt[i])
  }        
  data <- data.frame(x = 1:n.test, y = vec, col = as.factor(testing >= 500))
  tikz("SMPTestSetLokal.tex", width = 2.75, height = 2.75) 
  ggplot(data) + geom_point(aes(x = x, y = y, colour = col), alpha = 1, size = 1) + 
    xlab("index") + 
    scale_colour_manual (values = c("royalblue", "maroon"), legend=FALSE) +
    ylab("empirical small ball probability") + ylim(c(0,1)) + 
     opts(axis.title.y = theme_text(size = 8, angle=90), 
          axis.title.x = theme_text(size = 8, vjust=0),
          axis.text.x = theme_text(size = 6),
          axis.text.y= theme_text(size = 6))
  dev.off()
}  
  