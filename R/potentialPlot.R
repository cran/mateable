
##' Visualize mating potential
##'
##' @title graphical visualization of a mating potential object
##' @param matPot a mating potential object
##' @param subject a character string indicating whether the subject to be visualized is individuals or all pairwise interactions
##' @param plotType a character string indivating the plots to be displayed. Options are histogram ('hist'), network diagram ('net')
##' @param showDensity logical. If true (default), plots probability density over histogram.
##' @param sub.ids a vector containing the ids of individuals to be represented in pairwise potential plots
##' @param N a positive number, the number of individuals to sample if sub.ids = 'random'
##' @param sample a character string specifying how to choose a subset of individuals to be represented in pairwise potential plots. Possible values are "random" (default) or "all".
##' @param lab.cex parameter indicating label size relative to plot
##' (different than cex.lab)
##' @param main the main title (on top of plot)
##' @param ... optional arguments for the plot function
##' @export
##' @author Amy Waananen
##' @seealso see generic function \code{\link{points}} for values of \code{pch}
##' @examples
##' pop <- simulateScene()
##' sync <- synchrony(pop, "augs")
##' plotPotential(sync)
##'
##'
plotPotential <-   function(matPot,
                            subject = c('pair','ind'),
                            plotType = 'auto',
                            showDensity = T,
                            sub.ids = NULL, N = 9, sample = "random",
                            lab.cex = 0.5, main = NULL, ...){

  nm <- par("mar")
  noma <- par('oma')
  nmfrow <- par('mfrow')

  if(!sample %in% c("random", "all")) {warning("sample must be 'random' or 'all'")}

  pt <- match.arg(plotType, c('auto','heat','net','hist'), several.ok = TRUE)

  if (!is.list(matPot[[1]])){
    matPot <- list(matPot)
  }

  if (!is.null(subject)){
    if (!'pair' %in% names(matPot[[1]])){
      subject <- 'ind'
    } else {
      subject <- 'pair'
    }
  }

  if(attr(matPot[[1]],'t')){
    potential <- 'synchrony'
  } else if(attr(matPot[[1]],'s')){
    potential <- 'proximity'
  } else if(attr(matPot[[1]],'c')){
    potential <- 'compatibility'
  }

  if(!'pair' %in% names(matPot[[1]])){
    if('pair' %in% subject){
      warning('potentials object must have pairwise potential for subject to be pairwise interactions')
      subject <- 'ind'
    }
  }

  if ('auto' %in% pt){
    if (subject %in% 'pair'){
      pt <- c('heat','hist','net')
    } else {pt <- 'hist'}
  }

  if(is.null(main) & subject %in% 'ind') main <- paste('individual', potential)
  if(is.null(main) & subject %in% 'pair') main <- paste('pairwise', potential)

  nr <- length(matPot)
  nc <- length(pt)

  if (subject %in% 'ind'){
    par(mfrow = c(nr,1))
    par(oma = c(0,0,0,0))
  } else {
    par(mfrow = c(nr,nc))
    par(mar = c(4,0.5,0.5,2.5))
    par(oma = c(4,4,4,1.5))
  }

  ids <- matPot[[1]][['ind']][['id']]

  if (is.null(sub.ids)){
    if (sample %in% 'all'){
      sub.ids <- ids
    } else {
      sub.ids <- sample(ids, N)
    }
  }

  if ('ind' %in% subject){
    hmax <- max(hist(matPot[[1]][[subject]][,potential], breaks = 15, plot = F)$breaks)
    hmin <- min(hist(matPot[[1]][[subject]][,potential], breaks = 15, plot = F)$breaks)
  } else {
    hmax <- max(hist(matPot[[1]][[subject]], breaks = 15, plot = F)$breaks)
    hmin <- min(hist(matPot[[1]][[subject]], breaks = 15, plot = F)$breaks)
  }


  for (j in 1:length(matPot)){
    potj <- matPot[[j]]
    if('hist' %in% pt){
      if('ind' %in% subject){
        if (max(hist(potj[[subject]][,potential], breaks = 15, plot = F)$breaks) > hmax){
          hmax <- max(hist(potj[[subject]][,potential], breaks = 15, plot = F)$breaks)
        }
        if (min(hist(potj[[subject]][,potential], breaks = 15, plot = F)$breaks) < hmin){
          hmin <- min(hist(potj[[subject]][,potential], breaks = 15, plot = F)$breaks)
        }
      } else {
        if (max(hist(potj[[subject]], breaks = 15, plot = F)$breaks) > hmax){
          hmax <- max(hist(potj[[subject]], breaks = 15, plot = F)$breaks)
        }
        if (min(hist(potj[[subject]], breaks = 15, plot = F)$breaks) < hmin){
          hmin <- min(hist(potj[[subject]], breaks = 15, plot = F)$breaks)
        }
      }
    }
  }

  for (i in 1:length(matPot)){
    poti <- matPot[[i]]
    iids <- poti[['ind']][['id']]
    sub.iids <- poti[['ind']][which(iids %in% sub.ids), 'id']

    if (subject %in% 'pair'){
      diag(poti[['pair']]) <- 1
      subMat<- poti[['pair']][which(sub.iids %in% attr(poti[['pair']],'idOrder')),which(sub.iids %in% attr(poti[['pair']],'idOrder'))]

      if ('hist' %in% pt){
        hist(poti[['pair']], breaks = 15, prob = T, xlab = NULL, main = NULL, ylab = "")
        mtext(names(matPot)[i],side = 2,adj = 0.5, cex = 0.75, line = 3, font = 2)
        if (i == nr){
          title(xlab = potential)
        }
        if (showDensity){
          lines(density(poti[['pair']]))
        }
      }

      if ('net' %in% pt){
        if(length(sub.iids)< 3){
          plot(1, type="n", axes=F, xlab="", ylab="")
        } else {
          subMat[upper.tri(subMat, diag = TRUE)] <- 0
          im <- poti[['ind']][which(sub.iids %in% iids), potential]
          lab.cex <- 1 + (im - min(im))/(max(im) - min(im))
          plot_web3(subMat, names = sub.iids, val = FALSE, legend = FALSE, length = 0,
                    labz.size = lab.cex, ...)
        }
        if (! 'hist' %in% pt){
          mtext(names(matPot)[i],side = 2,adj = 0.5, cex = 0.75, las = 1, font = 2)
        }
      }



      if ('heat' %in% pt){
        if(length(sub.iids) <= 2){
          plot(1, type="n", axes=F, xlab="", ylab="")
        } else {
          diag(subMat) <- 1
          subMat[upper.tri(subMat, diag = FALSE)] <- NA
          image(x = 1:nrow(subMat),y = 1:nrow(subMat), z = subMat, axes = F, xlab = "", ylab = "", col = colorRampPalette(c('white','red'))(12))
          legend("topleft", legend = round(seq(min(subMat, na.rm = T),max(subMat, na.rm = T),length.out = 12),digits = 2), fill = colorRampPalette(c('white','red'))(12),ncol = 3, bty = 'n')
          axis(1, 1:ncol(subMat), labels = sub.iids, tick = 0, cex.axis = -0.2 + 1/log10(nrow(subMat)))
          axis(4, 1:ncol(subMat), labels = sub.iids, tick = 0, cex.axis = -0.2 + 1/log10(nrow(subMat)), las = 2)
          par(mgp = c(3,1,0))
        }
        if(!'hist'%in% pt &! 'net' %in% pt){
          mtext(names(matPot)[i],side = 2,adj = 0.5, cex = 0.75, las = 1, font = 2)
        }
      }



    } else if (subject %in% 'ind') {
      par(mar = c(4,4,4,1))
      if ('hist' %in% pt){
        hist(poti[[subject]][,potential], prob = TRUE, breaks = 15, main = NULL, axes = F, xlab = NULL,xlim = c(hmin,hmax), ylab = NULL)
        title(ylab = 'density', outer = T, line = -1.5)
        axis(1)
        axis(2)
        mtext(names(matPot)[i],side = 2,adj = 0.5, cex = 0.75, line = 5, font = 2)
        mtext(main, side = 3, line = 4, cex = 1.5)
        if (i == nr){
          par(mar = c(4,3,1,1))
          title(xlab = potential)
        }
        if (showDensity){
          lines(density(poti[[subject]][,potential]))
        }
      }
    }
  }

  title(main = main, outer = T)
  par(mar = nm, mfrow = nmfrow, oma = noma)
}



plot_web3 <-
  function (flowmat, names = NULL, lab.size = 1.5, add = FALSE,
            fig.size = 1.3, main = "", sub = "", sub2 = "", log = FALSE,
            mar = c(0.25, 0.25, 0.25, 0.25), nullflow = NULL, minflow = NULL, maxflow = NULL,
            legend = TRUE, leg.digit = 5, leg.title = NULL, lcol = "black",
            arr.col = "black", val = FALSE, val.digit = 5, val.size = 0.6,
            val.col = "red", val.title = NULL, val.ncol = 1, budget = FALSE,
            bud.digit = 5, bud.size = 0.6, bud.title = "budget", bud.ncol = 1,
            maxarrow = 10, minarrow = 1, length = 0.1, dcirc = 1.2, bty = "o",
            labz.size = 1.5, ...)
  {
    nm <- par("mar")
    if (ncol(flowmat) != nrow(flowmat))
      stop("flowmat has to be square")
    components <- names
    if (is.null(components))
      components <- colnames(flowmat)
    if (is.null(components))
      components <- rownames(flowmat)
    if (is.null(components))
      components <- as.character(1:ncol(flowmat))
    numcomp <- length(components)
    if (ncol(flowmat) != numcomp)
      stop("flowmat and names not compatible")
    if (length(arr.col) == 1)
      arr.col <- matrix(nrow = numcomp, ncol = numcomp, arr.col)
    flowmatrix <- flowmat
    if (!is.null(nullflow)) {
      flowmatrix[flowmatrix < nullflow[1]] <- 0
      if (length(nullflow == 2))
        flowmatrix[flowmatrix > nullflow[2]] <- 0
    }
    zero <- 0
    if (log) {
      flowmatrix <- log10(flowmatrix + 1e-20)
      flowmatrix[flowmatrix == -20] <- 0
    }
    if (is.null(maxflow))
      maxflow <- max(flowmatrix)
    else if (log)
      maxflow <- log10(maxflow)
    if (is.null(minflow))
      minflow <- min(flowmatrix[flowmatrix != zero])
    else if (log)
      minflow <- log10(minflow)
    if (!add) {
      figlim <- c(-fig.size, fig.size)
      marg <- par("mar")
      if (val)
        mar <- mar + c(0, -2, 0, 2)
      mar <- pmax(mar, 0)
      par(mar = mar)
      plot(c(0, 0), type = "n", ylab = "", asp = 1, xaxt = "n",
           yaxt = "n", frame.plot = FALSE, xlim = figlim, ylim = figlim,
           main = main, xlab = "")
      mtext(side = 3, line = -1, sub)
      mtext(side = 1, adj = 0.5, text = sub2)
    }
    alpha0 <- pi/2
    alpha <- alpha0 - (1:numcomp) * 2 * pi/numcomp
    xl <- cos(alpha)
    yl <- sin(alpha)
    if(length(labz.size) == 1) labz.size <- rep(labz.size, numcomp)
    for (i in 1:numcomp) {
      if (xl[i] > 0)
        adjustx = 0
      if (xl[i] < 0)
        adjustx = 1
      if (abs(xl[i]) < 1e-04)
        adjustx = 0.5
      if (yl[i] > 0)
        adjusty = 0
      if (yl[i] < 0)
        adjusty = 1
      if (abs(yl[i]) < 1e-04)
        adjusty = 0.5
      text(xl[i], yl[i], components[i], adj = c(adjustx, adjusty),
           cex = par("cex") * labz.size[i])
    }
    circle <- function(i, lwd, col) {
      cx <- xl[i] * dcirc
      cy <- yl[i] * dcirc
      r <- 0.1
      x <- c(seq(-pi, pi, by = 0.01), pi)
      lines(cx + r * sin(x), cy + r * cos(x), lwd = lwd, col = col)
    }
    par(lend = 1)
    darrow <- (maxarrow - minarrow)/(maxflow - minflow)
    dr <- 0.02
    xi <- xl - dr * cos(alpha)
    yi <- yl - dr * sin(alpha)
    iflow <- 1
    offset <- 1
    ltext <- NULL
    for (i in 1:numcomp) {
      x2 <- xi[i]
      y2 <- yi[i]
      for (j in 1:i) {
        if (flowmatrix[i, j] > zero | flowmatrix[j, i] >
            zero) {
          Arr.col <- arr.col[i, j]
          x1 <- xi[j]
          y1 <- yi[j]
          dx <- x2 - x1
          dy <- y2 - y1
          ifelse(i == j, fsize <- flowmatrix[i, j], fsize <- flowmatrix[i,
                                                                        j] - flowmatrix[j, i])
          if (fsize > 0) {
            code <- 1
          }
          else {
            code <- 2
            Arr.col <- arr.col[j, i]
          }
          size <- minarrow + darrow * (abs(fsize) - minflow)
          if (i != j)
            arrows(x1 + dr * dx, y1 + dr * dy, x2 - dr *
                     dx, y2 - dr * dy, length = length, code = code,
                   lwd = size, col = Arr.col, ...)
          if (i == j)
            circle(i, lwd = size, col = Arr.col)
          if (val) {
            text(x = (x1 + x2) * 0.5, y = (y1 + y2) * 0.5,
                 labels = iflow, offset = offset, col = val.col)
            ltext <- c(ltext, paste(iflow, ":", format.pval(abs(fsize),
                                                            val.digit)))
          }
          iflow <- iflow + 1
        }
      }
    }
    if (legend) {
      sizeleg = par("cex") * lab.size
      if (!log) {
        tmax <- maxflow
        tmin <- minflow
        title = leg.title
      }
      else {
        tmax <- 10^maxflow
        tmin <- 10^minflow
        title = paste("logarithmic scale", leg.title)
      }
      legend("bottomright", legend = c(format.pval(tmax, leg.digit),
                                       format.pval(tmin, leg.digit)), cex = sizeleg, title = title,
             lwd = c(maxarrow, minarrow), bty = bty)
    }
    if (!val & !budget)
      return
    if (!add) {
      par(mar = c(0, 0, 0, 0))
      par(new = TRUE)
      plot(c(0, 0), type = "n", ylab = "", xaxt = "n", yaxt = "n",
           frame.plot = FALSE, main = "", xlab = "")
    }
    if (val)
      legend("topright", legend = ltext, cex = val.size, title = val.title,
             ncol = val.ncol, bty = bty)
    if (budget) {
      rate <- NULL
      for (i in 1:numcomp) rate <- c(rate, paste(components[i],
                                                 ":", format.pval(sum(flowmat[, i]) - sum(flowmat[i,
                                                                                                  ]), bud.digit)))
      legend("topleft", legend = rate, cex = bud.size, title = bud.title,
             ncol = bud.ncol, bty = bty)
    }
    par(mar = nm)
  }

