##'
##' Visualize mating potential
##'
##' @title graphical visualization of a mating potential object
##' @param matPot a mating potential object
##' @param subject character, either 'ind' or 'pair', indicating whether the subject being visualized is individuals or pairwise interactions
##' @param plotType character,  indicating what plots are to be displayed. See details. Options are histogram ('hist'), network diagram ('net'), and heatmap ('heat'). If mating potential object
##' @param density logical. If TRUE (default), plots probability density over histogram.
##' @param sub.ids a vector containing the ids of individuals to be represented in pairwise potential plots
##' @param N a positive number indicating the number of individuals to sample if sub.ids = 'random'
##' @param sample a character string specifying how to choose a subset of individuals to be represented in pairwise potential plots. Possible values are "random" (default) or "all" (see details).
##' @param ... optional arguments for the plot function
##' @return No return value, called to draw plots
##' @details Options for \code{plotType} are 'hist' (histogram), 'net' (network diagram), 'heat' (heatmap), and 'auto'. Default value is 'auto':
##' if the mating potential object contains pairwise potential, 'auto' returns all plot types, otherwise it returns histograms of individual potential.
##' @details The individuals to be represented in the pairwise potential plots can either be specified explicitly through \code{sub.ids}, chosen randomly
##'  (\code{sample} = 'random'), or all individuals can be selected (\code{sample} = 'all'). The default is to randomly select 9 individuals.
##'  If multiple years are being plotted, the subset is sampled from all years and the same individuals will be represented in each year, if possible.
##'  If fewer than three individuals from the subset are available in a year, no network diagram or heatmap will be returned for that year.
##' @export
##' @author Amy Waananen
##' @seealso see generic function \code{graphics::\link[graphics]{points}()} for values of \code{pch}
##' @examples
##' pop <- simulateScene()
##' sync <- synchrony(pop, "augs")
##' plotPotential(sync)
plotPotential <-   function(matPot,
                            subject = NULL,
                            plotType = 'auto',
                            density = TRUE,
                            sub.ids = NULL, N = 9, sample = "random", ...){

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  if(!sample %in% c("random", "all")) {warning("sample must be 'random' or 'all'")}

  pt <- match.arg(plotType, c('auto','heat','net','hist'), several.ok = TRUE)

  if (!is.list(matPot[[1]])){
    matPot <- list(matPot)
  }

  if (is.null(subject)){
    subject <- ifelse('pair' %in% names(matPot[[1]]), 'pair','ind')
  }

  if(attr(matPot[[1]],'t')){
    potential <- 'synchrony'
  } else if(attr(matPot[[1]],'s')){
    if(!is.null(matPot[[1]][['ind']]) & 'knn.dist' %in% colnames(matPot[[1]][['ind']])){
      potential <- 'knn.dist'
    } else {
      potential <- 'proximity'
    }
  } else if(attr(matPot[[1]],'c')){
    potential <- 'compatibility'
  }
  if(!'pair' %in% names(matPot[[1]]) & 'pair' %in% subject){
    warning("mating potential object must have pairwise potential for subject to be 'pair'")
    subject <- 'ind'
  }
  print (potential)
  if ('auto' %in% pt){
    if (subject %in% 'pair'){
      pt <- c('heat','hist','net')
    } else {pt <- 'hist'}
  }

  # if(is.null(main) & subject %in% 'ind') main <- paste('individual', potential)
  # if(is.null(main) & subject %in% 'pair') main <- paste('pairwise', potential)

  nr <- length(matPot)
  nc <- length(pt)

  # if (subject %in% 'ind'){
  #   par(mfrow = c(nr,1), mar = c(4,6,0,1))
  #   if (nr > 1){
  #     par(oma = c(1,1,2,1))
  #   } else {
  #     par(oma = c(1,0,2,1))
  #   }
  # } else {
  #   par(mfrow = c(nr,nc), mar = c(4,0.5,0.5,2.5),oma = c(4,4,4,4) )
  # }

  if (is.null(sub.ids)){
    if(sample == 'random'){
      sub.ids <- sample(unique(unlist(sapply(matPot, function(x)x$ind$id))),N)
    } else if(sample == 'all'){
      sub.ids <-unique(unlist(sapply(matPot, function(x)x$ind$id), use.names = FALSE))
    }
  }

  if ('ind' %in% subject){
    hmax <- max(unlist(lapply(matPot,function(x)hist(x[[subject]][,potential], breaks = 15, plot = FALSE)$breaks)))
    hmin <- min(unlist(lapply(matPot,function(x)hist(x[[subject]][,potential], breaks = 15, plot = FALSE)$breaks)))
  } else {
    hmax <- max(unlist(lapply(matPot,function(x)hist(x[[subject]], breaks = 15, plot = FALSE)$breaks)))
    hmin <- min(unlist(lapply(matPot,function(x)hist(x[[subject]], breaks = 15, plot = FALSE)$breaks)))
  }

  for (i in 1:length(matPot)){
    poti <- matPot[[i]]
    iids <- poti[['ind']][['id']]
    sub.iids <- poti[['ind']][which(iids %in% sub.ids), 'id']

    if (subject %in% 'pair'){
      diag(poti[['pair']]) <- 1
      subMat<- poti[['pair']][which(attr(poti[['pair']],'idOrder') %in% sub.iids),which(attr(poti[['pair']],'idOrder') %in% sub.iids)]

      if ('hist' %in% pt){
        hist(poti[['pair']], breaks = 15, prob = TRUE, xlab = NULL, main = NULL, ylab = "")
        mtext(names(matPot)[i],side = 2,adj = 0.5, cex = 0.75, line = 3, font = 2)
        if (i == nr){
          title(xlab = potential)
        }
        if (density){
          lines(density(poti[['pair']], na.rm = TRUE))
        }
      }

      if ('net' %in% pt){
        if(length(sub.iids)< 3){
          plot(1, type="n", axes=FALSE, xlab="", ylab="")
        } else {
          subMat[upper.tri(subMat, diag = TRUE)] <- 0
          im <- poti[['ind']][which(iids %in% sub.iids), potential]
          lab.cex <- 1 + (im - min(im))/(max(im) - min(im))
          if(sum(subMat >= 1) > 4){
            plot_net(subMat, names = sub.iids, minflow = 0, maxarrow = 3, minarrow = 1,
                     labz.size = lab.cex, ...)
          } else {
            plot_net(subMat, names = sub.iids, minflow = 0,
                     labz.size = lab.cex, ...)
          }
        }
        if (! 'hist' %in% pt){
          mtext(names(matPot)[i],side = 2,adj = 0.5, cex = 0.75, las = 1, font = 2)
        }
      }

      if ('heat' %in% pt){
        if(length(sub.iids) <= 2){
          plot(1, type="n", axes=FALSE, xlab="", ylab="")
        } else {
          if (potential == "compatibility") {
            diag(subMat) <- 0
          } else {
            diag(subMat) <- 1
          }
          subMat[upper.tri(subMat, diag = FALSE)] <- NA
          leg.labs <- round(seq(max(subMat, na.rm = TRUE),min(subMat, na.rm = TRUE),length.out = 9), digits = 2)
          image(x = 1:nrow(subMat),y = 1:nrow(subMat), z = subMat, axes = FALSE, xlab = "", ylab = "", col = colorRampPalette(c('white','red'))(9))
          legend("topleft", legend = c(leg.labs[1],'','','',leg.labs[5],'','','',leg.labs[9]), fill = colorRampPalette(c('red','white'))(9), bty = 'n', y.intersp = 0.65, pt.cex = 1.2)
          axis(1, 1:ncol(subMat), labels = sub.iids, tick = 0, cex.axis = -0.2 + 1/log10(nrow(subMat)), las = 3)
          axis(4, 1:ncol(subMat), labels = sub.iids, tick = 0, cex.axis = -0.2 + 1/log10(nrow(subMat)), las = 2)
        }
        if(!'hist'%in% pt &! 'net' %in% pt){
          mtext(names(matPot)[i],side = 2,adj = 0.5, cex = 0.75, las = 1, font = 2)
        }
      }

    } else if (subject %in% 'ind') {
      if ('hist' %in% pt){
        hist(poti[[subject]][,potential], prob = TRUE, breaks = 15, main = NULL, axes = FALSE, xlab = NULL,xlim = c(hmin,hmax), ylab = NULL)
        title(ylab = 'density')
        axis(1)
        axis(2)
        if (nr > 1){
          mtext(names(matPot)[i],side = 2,adj = 0.5, cex = 0.75, line = 5, font = 2)
        }
        if (i == nr){
          title(xlab = potential)
        }
        if (density){
          lines(density(poti[[subject]][,potential], na.rm = TRUE))
        }
        if(potential %in% 'knn.dist'){
          mtext(paste('k = ',matPot[[1]][['ind']][1,'k']), side = 3, adj = 0.95, outer = TRUE)
        }
      }
    }
  }
  # mtext(main, side = 3, line = 0, cex = 1.5, outer = TRUE)
}



plot_net <- function (flowmat, names = NULL, add = FALSE,
                      fig.size = 1.7, mar = c(0.25, 0.25, 0.25, 0.25), nullflow = NULL, minflow = NULL, maxflow = NULL,
                      maxarrow = 10, minarrow = 1, bty = "o",
                      labz.size = 0.5, xpd= TRUE, ...){

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  components <- names
  numcomp <- length(components)
  flowmatrix <- flowmat
  if (!is.null(nullflow)) {
    flowmatrix[flowmatrix < nullflow[1]] <- 0
    if (length(nullflow == 2))
      flowmatrix[flowmatrix > nullflow[2]] <- 0
  }
  zero <- 0
  if (is.null(maxflow))
    maxflow <- max(flowmatrix)
  if (is.null(minflow))
    minflow <- min(flowmatrix[flowmatrix != zero])
  figlim <- c(-fig.size, fig.size)
  # mar <- pmax(mar, 0)
  par(mar = mar)
  plot(c(0, 0), type = "n", ylab = "", asp = 1, xaxt = "n",
       yaxt = "n", frame.plot = FALSE, xlim = figlim, ylim = figlim,
       main = '', xlab = "")
  alpha <- (pi/2 - (1:numcomp) * 2 * pi/numcomp)
  xl <- cos(alpha)
  yl <- sin(alpha)
  if(length(labz.size) == 1) labz.size <- rep(labz.size, numcomp)
  for (i in 1:numcomp) {
    if (xl[i] > 0)
      adjustx = -0.25
      adjustx = 0
    if (xl[i] < 0)
      adjustx = 1.25
    if (abs(xl[i]) < 1e-04)
      adjustx = 0.75
    if (yl[i] > 0)
      adjusty = -0.25
    if (yl[i] < 0)
      adjusty = 1.25
    if (abs(yl[i]) < 1e-04)
      adjusty = 0.75
    text(xl[i] * 0.95 , yl[i] * 0.95 , components[i], adj = c(adjustx, adjusty),
         cex = par("cex") * labz.size[i], xpd = TRUE)
  }
  par(lend = 'round')
  darrow <- (maxarrow - minarrow)/(maxflow - minflow)
  xi <- xl - 0.02 * cos(alpha)
  yi <- yl - 0.02 * sin(alpha)
  for (i in 1:numcomp) {
    x2 <- xi[i]
    y2 <- yi[i]
    for (j in 1:i) {
      if (flowmatrix[i, j] > zero | flowmatrix[j, i] >
          zero) {
        x1 <- xi[j]
        y1 <- yi[j]
        dx <- x2 - x1
        dy <- y2 - y1
        fsize <- flowmatrix[i,j] - flowmatrix[j, i]
        size <- minarrow + darrow * (abs(fsize) - minflow)
        arrows(x1 + 0.02 * dx, y1 + 0.02 * dy, x2 - 0.02 *dx, y2 - 0.02 * dy, length = 0, lwd = size, col = 'black', ...)
      }
    }
  }
}

