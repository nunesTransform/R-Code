#################################################################################
#
# author: Simon Müller
# date: 14.4.2012
# describtion: functions for converting osmar lines to object that can be 
# plotted with geom_segments ggplot2
#  
#
#################################################################################

conv_sp_lines_to_seg <- function(lines) {
  as.data.frame(do.call(rbind, 
                        mclapply(mclapply(slot(lines, "lines"), fun1), get_line)))
}

fun1 <- function(x) {
  as.data.frame(mclapply(slot(x, "Lines"), fun2))
}

fun2 <- function(y) {
  as.data.frame(slot(y, "coords"))
}

get_line <- function(x) {
    l <- data.frame(x)
    n <- nrow(l)
    return(data.frame(slon=l[-n, 1], slat=l[-n, 2], 
                      elon = l[-1, 1], elat = l[-1, 2]))
}

