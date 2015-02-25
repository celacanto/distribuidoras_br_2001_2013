# Packages
# -------------------------------------------------------
library(plotrix)
library(RColorBrewer)
library(ellipse)
library(caroline)
library(asbio)




# Change  the violins function (from caroline to add diferent borders colors)
violins2 <- function (x, by, range = 1.5, h = NULL, ylim = NULL, names = NULL, 
                       horizontal = FALSE, col = "transparent", border.col = "black", 
                       lty = 1, lwd = 1, at, add = FALSE, wex = 1, drawRect = TRUE, main = "", 
                       xlab = "", ylab = "", 
                      las = 2, quantiles = c(0.1, 0.9), deciles = TRUE) 
{
  options(warnings = -1)
  require(sm)
  if (is.data.frame(x)) 
    x <- as.list.data.frame(x)
  if (!missing(by)) {
    if (is.numeric(by)) 
      x <- .cat2list(x[order(by)], sort(by))
    if (!is.numeric(by)) 
      x <- .cat2list(x, by)
  }
  if (is.list(x)) {
    datas <- x
    if (length(names) == 0) 
      names <- names(x)
  }
  else {
    datas <- list(x)
  }
  n <- length(datas)
  if (missing(at)) 
    at <- 1:n
  
  border  <- rep(border.col, length.out = n)
  
  upper <- vector(mode = "numeric", length = n)
  lower <- vector(mode = "numeric", length = n)
  q.1 <- vector(mode = "numeric", length = n)
  q1 <- vector(mode = "numeric", length = n)
  q3 <- vector(mode = "numeric", length = n)
  q.9 <- vector(mode = "numeric", length = n)
  med <- vector(mode = "numeric", length = n)
  hubermu <- vector(mode = "numeric", length = n)
  average <- vector(mode = "numeric", length = n)
  stddevlower <- vector(mode = "numeric", length = n)
  stddevupper <- vector(mode = "numeric", length = n)
  stderrlower <- vector(mode = "numeric", length = n)
  stderrupper <- vector(mode = "numeric", length = n)
  base <- vector(mode = "list", length = n)
  height <- vector(mode = "list", length = n)
  medCI05 <- vector(mode = "list", length = n)
  medCI95 <- vector(mode = "list", length = n)
  decile <- matrix(NA, nrow = n, ncol = 9)
  baserange <- c(Inf, -Inf)
  args <- list(display = "none")
  if (!(is.null(h))) 
    args <- c(args, h = h)
  for (i in 1:n) {
    data <- (datas[[i]])
    data.min <- min(data, na.rm = TRUE)
    data.max <- max(data, na.rm = TRUE)
    q.1[i] <- quantile(data, quantiles[1], na.rm = TRUE)
    q1[i] <- quantile(data, 0.25, na.rm = TRUE)
    q3[i] <- quantile(data, 0.75, na.rm = TRUE)
    q.9[i] <- quantile(data, quantiles[2], na.rm = TRUE)
    med[i] <- median(data, na.rm = TRUE)
    medCI05[i] <- ci.median(data)$ci[2]
    medCI95[i] <- ci.median(data)$ci[3]
    hubermu[i] <- huber.mu(data)
    average[i] <- mean(data)
    iqd <- q3[i] - q1[i]
    upper[i] <- min(q3[i] + range * iqd, data.max)
    lower[i] <- max(q1[i] - range * iqd, data.min)
    stddevlower[i] <- average[i] - sd(data)
    stddevupper[i] <- average[i] + sd(data)
    if (deciles) 
      for (j in 1:9) decile[i, j] <- quantile(data, j/10)
    N <- length(data)
    stderrlower[i] <- average[i] - (sd(data)/sqrt(N))
    stderrupper[i] <- average[i] + (sd(data)/sqrt(N))
    est.xlim <- c(min(lower[i], data.min), max(upper[i], 
                                               data.max))
    smout <- do.call("sm.density", c(list(data, xlim = est.xlim), 
                                     args))
    hscale <- 0.4/max(smout$estimate) * wex
    base[[i]] <- smout$eval.points
    height[[i]] <- smout$estimate * hscale
    t <- range(base[[i]])
    baserange[1] <- min(baserange[1], t[1])
    baserange[2] <- max(baserange[2], t[2])
  }
  if (!add) {
    xlim <- if (n == 1) 
      at + c(-0.5, 0.5)
    else range(at) + min(diff(at))/2 * c(-1, 1)
    if (is.null(ylim)) {
      ylim <- baserange
    }
  }
  if (is.null(names)) {
    label <- 1:n
  }
  else {
    label <- names
    if (length(at) == 1) 
      at <- 1:n + at
  }
  boxwidth <- 0.05 * wex
  
  if (!add) plot.new()
  if (!horizontal) {
    if (!add) {
      plot.window(xlim = xlim, ylim = ylim, las = las)
      title(main, xlab = xlab, ylab = ylab)
    }
    # box()
    for (i in 1:n) {
      polygon(c(at[i] - height[[i]], rev(at[i] + height[[i]])), 
              c(base[[i]], rev(base[[i]])), col = col[i], border = border[i], 
              lty = lty, lwd = lwd)
    }
  }
  else {
    if (!add) {
      plot.window(xlim = ylim, ylim = xlim, las = las)
      axis(1, las = las)
      axis(2, at = at, labels = label, las = las)
    }
    box()
    for (i in 1:n) {
      polygon(c(base[[i]], rev(base[[i]])), c(at[i] - height[[i]], 
                                              rev(at[i] + height[[i]])), col = col[i], border = border[i], 
              lty = lty, lwd = lwd)
    }
  }
}



###############33 PLOT

myViolinsPlot  <- function(x, 
                      mar = c(5.1, 6 ,4.1,2.1),
                      
                      yLim = NULL,
                      
                      points = TRUE, 
                      pch.points = "-",
                      cex.points = 1, 
                      col.points = "#1C201F",
                
                      outline = FALSE,
                      col.violins = "#1C201F",
                      lwd.violins = 0.5,

                      axis = TRUE,
                      col.axis  = "#1C201F", 
                      col.ticks = "#1C201F",
                      
                      labels.x = NULL,
                      cex.x = 0.9, 
                      
                      legend.y = "",
                      legend.y.cex = 1,
                      legendLine.y = 5,
                      
                      n.Yticks  = 10, 
                      yRound = 1,
                      cex.y = 0.9, 
                      line.y = 2,
                      lwd.y.line  = 0,
                      lwd.y.ticks = 0.5,
                      las.y = 2, 
                      
                      family = "Palatino"){
  
  op  <- par(family = family,  mar = mar)
  
  if(class(x) == "matrix"){
    x  <- unlist(apply(x, 2, list), recursive = FALSE)
  }
  
  listNoNa  <- lapply(x, na.omit)
  unlistNoNA  <- unlist(listNoNa)
  
  
  ### limits
  # X
  xLim  <- c(1, length(listNoNa))
  # Y
  if(is.null(yLim)){
    yLim  <- range(unlistNoNA)    
  }
  
  ### Labels
  # X
  if(is.null(labels.x)){
    labels.x  <- names(listNoNa)
  } else {
    labels.x  <- 1:length(listNoNa)
  }
  # Y
  ylabel  <- round(seq(from = yLim[1], to = yLim[2], length.out = n.Yticks), yRound) 
  
 #################33 
 
  plot(x = xLim + c(-0.5, 0.5), yLim, type = "n", axes = FALSE, ylab = "", xlab = "")
  
  col  <- col.violins
  boxplot(x = listNoNa, at = 1:xLim[2],
          main = "", ylab ="", xlab="", 
          axes = FALSE, 
          outline = outline, 
          xlim = xLim,
          ylim = yLim, 
          pars = list(boxlty = 0, 
                      staplewex = 0, whisklty = 1, whisklwd =0.7, whiskcol = col,
                      medpch= 19, medcex = 0.9, medcol = col, medlty = 0,
                      outpch= 19, outcex = 0.5, outcol = col),
          add = TRUE)
  violins2(listNoNa, add = TRUE, lwd = lwd.violins, border.col = col.violins)
 
  
  
  if(axis){
    yPos  <- axis(2,
                  at = ylabel, 
                  las = las.y,
                  line = line.y,  
                  col = col.axis,  
                  col.ticks = col.ticks,
                  cex.axis = cex.y,
                  lwd = lwd.y.line,
                  lwd.ticks = lwd.y.ticks)
    
     xPos  <- axis(1, at =  1:xLim[2],
                   labels = labels.x, 
                   col = col.axis,
                   lwd = 0,
                   cex.axis = cex.x)
  }
  
 if(points){
    points(y = unlist(listNoNa), x = rep(1:length(listNoNa), sapply(listNoNa, length)), pch = pch.points, cex = cex.points, 
           col = col.ticks)
 }
 
 mtext(text = legend.y, side = 2, line = legendLine.y, font = 2, cex  = legend.y.cex)
 
 par(op)
}


.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}
