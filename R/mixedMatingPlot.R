##' Visualize multiple dimensions of a mating scene
##'
##' @title multi-dimensional visualization of mating scene object
##' @param scene a matingScene object
##' @param dimension what dimension(s) of the mating scene should be visualized. Possible dimensions are 't' for temporal, 's' for spatial, 'mt' for mating type, and 'auto' (the default). For dimension = 'auto', all dimensions represented in the mating scene object will be plotted.
##' @param sub a subset of the population to plot
##' @param xcoord x-axis coordinate system label
##' @param ycoord y-axis coordinate system label
##' @param pch point type, defaults to pch = 19, solid filled in circle. If pch = NULL, individuals will be labeled by their id.
##' @param ... optional arguments for the plot function
##' @return nothing
##' @export
##' @author Amy Waananen
##' @seealso see generic function \code{\link{points}} for values of \code{pch}
##' @examples
##' pop <- simulateScene()
##' plot3DScene(pop)
##'
##'
##'
plot3DScene <- function(scene, dimension = "auto",
                            sub= NULL, ycoord = 'northing', xcoord = 'easting',
                            pch = 19, ...){
  dimension <- match.arg(dimension, c("auto", "t", "s", "mt"),several.ok = TRUE)
  nm <- par("mar")
  nmfrow <- par('mfrow')
  noma <- par('oma')

  if (is.list(scene) & !is.data.frame(scene)) {

    if ("auto" %in% dimension) {
      temp <- attr(scene[[1]], "t")
      spat <- attr(scene[[1]], "s")
      comp <- attr(scene[[1]], "mt")
    } else {
      temp <- F
      spat <- F
      comp <- F
      if ("t" %in% dimension) {
        temp <- T
      }
      if ("s" %in% dimension) {
        spat <- T
      }
      if ("mt" %in% dimension) {
        comp <- T
      }
    }

    if (!is.null(sub)){
      if(sub == 'random'){
        sub <- sample(unique(unlist(sapply(scene,function(x)x[,'id'], simplify = TRUE), use.names = F)),9)
      } else if(sub == 'all'){
        sub <-unique(unlist(sapply(scene,function(x)x[,'id'], simplify = TRUE), use.names = F))
      }
    }
    nr <- length(scene)
    par(mfrow = c(nr,1))
    par(oma = c(4,1,4,1))

    for (i in 1:length(scene)){
      if (spat){
        emin <- min(scene[[1]]['x'])
        emax <- max(scene[[1]]['x'])
        nmin <- min(scene[[1]]['y'])
        nmax <- max(scene[[1]]['y'])

        if (min(scene[[i]]['x'])< emin){
          emin <- min(scene[[i]][,'x'])
        }
        if (max(scene[[i]]['x'])> emax){
          emax <- max(scene[[i]][,'x'])
        }
        if (min(scene[[i]]['y'])< nmin){
          nmin <- min(scene[[i]][,'y'])
        }
        if (max(scene[[i]]['y'])> nmax){
          nmax <- max(scene[[i]][,'y'])
        }
      }

      if(comp){
        smin <- min(as.numeric(scene[[1]][['s1']]))
        smax <- min(as.numeric(scene[[1]][['s1']]))

        if (max(as.numeric(scene[[i]][['s1']])>smax)){
          smax <- max(as.numeric(scene[[i]][['s1']]))
        }
        if (max(as.numeric(scene[[i]][['s2']])>smax)){
          smax <- max(as.numeric(scene[[i]][['s2']]))
        }
        if (min(as.numeric(scene[[i]][['s1']])<smin)){
          smin <- min(as.numeric(scene[[i]][['s1']]))
        }
        if (min(as.numeric(scene[[i]][['s2']])<smin)){
          smin <- min(as.numeric(scene[[i]][['s2']]))
        }
      }

      if(temp){
          opening <- attr(scene[[1]],'origin')+min(scene[[1]][,'start'])
          if (attr(scene[[1]],'origin')+min(scene[[i]]['start']) < opening){
            opening <- attr(scene[[1]],'origin')+min(scene[[i]][,'start'])
        }
          closing <- attr(scene[[1]],'origin')+max(scene[[1]][,'end'])
          if (attr(scene[[1]],'origin')+max(scene[[i]][,'end']) > closing){
            closing <- attr(scene[[1]],'origin')+max(scene[[i]][,'end'])
        }

        maxstart <- max(scene[[1]]['start'])
        if (max(scene[[i]]['start']) > maxstart){
          maxstart <-max(scene[[i]]['start'])
        }

        minstart <- min(scene[[1]]['start'])
        if (min(scene[[i]]['start']) < minstart){
          minstart <-min(scene[[i]]['start'])
        }
        vec <- seq(minstart, maxstart, length.out = 9)
      }
    }

    for (i in 1:length(scene)){
      scene.i <- scene[[i]]
      if (temp){
        scene.i$cols <- findInterval(scene.i$start,vec)
        palette(colorRampPalette(c('blue','red'))(9))
      }
      if (temp & spat & comp){
        xcoord <- 'easting'
        ycoord <- 'northing'
        par(mar = c(1,9,1,1), xpd = T)
        plot.default(scene.i[, 'x'], scene.i[, 'y'], type = "n", ylab = ycoord, xaxt = 'n', xlim = c(emin,emax),ylim = c(nmin,nmax), ...)
        mtext(names(scene)[i],side = 2, cex = 0.75, font = 2, las = 1, adj = 0, line = 8)
        if (i == 1){
          title(main = 'spatial, temporal, and mating type plot', outer = T)
          if (is.null(pch)) {
            palette(colorRampPalette(c('blue','red'))(9))
            text(scene.i[, 'x'], scene.i[, 'y'], scene.i[, 'id'], col = scene.i$cols, ...)
          } else {
            text (scene.i[,'x'], scene.i[,'y'], paste(scene.i[,'s1'],', ',scene.i[,'s2'], sep = ""), pos = 2, cex = 0.9)
            if (!is.null(sub)){
              scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
              text(scene.i.sub[, 'x'], scene.i.sub[, 'y'], scene.i.sub[, 'id'], pos = 3, cex =1.1, font = 2, ...)
              points(scene.i.sub[, 'x'], scene.i.sub[, 'y'], pch = pch,cex= 1.4,col = scene.i.sub$cols,  ...)
              points(scene.i[!scene.i[, 'id'] %in% sub, 'x'], scene.i[!scene.i[, 'id'] %in% sub, 'y'], pch = pch, col = scene.i$cols, ...)
            } else {
              points(scene.i[, 'x'], scene.i[, 'y'], pch = pch, col = scene.i$cols, ...)
            }
          }
        } else if(i == nr){
          mtext('easting',side = 1,adj = 0.5, cex = 0.75, line = 3)
          axis(1)
          if (is.null(pch)) {
            text(scene.i[, 'x'], scene.i[, 'y'], scene.i[, 'id'], col = scene.i$cols, ...)
          } else {
            text (scene.i[,'x'], scene.i[,'y'], paste(scene.i[,'s1'],', ',scene.i[,'s2'], sep = ""), pos = 2, cex = 0.9)
            if (!is.null(sub)){
              scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
              text(scene.i.sub[, 'x'], scene.i.sub[, 'y'], scene.i.sub[, 'id'], pos = 3, cex =1.1, font = 2, ...)
              points(scene.i.sub[, 'x'], scene.i.sub[, 'y'], pch = pch,cex= 1.4,col = scene.i.sub$cols,  ...)
              points(scene.i[!scene.i[, 'id'] %in% sub, 'x'], scene.i[!scene.i[, 'id'] %in% sub, 'y'], pch = pch, col = scene.i$cols, ...)
            } else {
              points(scene.i[, 'x'], scene.i[, 'y'], pch = pch, col = scene.i$cols, ...)
            }
          }
          par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
          plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
          legend('topleft', legend = c(format(opening, format = "%b %d"),' ',' ',' ',
                                       format(attr(scene.i,'origin')+round(mean(c(minstart,maxstart))),format = "%b %d"),'',' ',' ',format(attr(scene.i,'origin')+maxstart, format = "%b %d")),
                 fill = colorRampPalette(c('blue','red'))(9),ncol = 1, bty = 'n',xpd = T, y.intersp = 0.68, title = 'start date', inset = c(0.02,0.03))
        } else {
          if (is.null(pch)) {
            text(scene.i[, 'x'], scene.i[, 'y'], scene.i[, 'id'], col = scene.i$cols, ...)
          } else {
            text (scene.i[,'x'], scene.i[,'y'], paste(scene.i[,'s1'],', ',scene.i[,'s2'], sep = ""), pos = 2, cex = 0.9)
            if (!is.null(sub)){
              scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
              text(scene.i.sub[, 'x'], scene.i.sub[, 'y'], scene.i.sub[, 'id'], pos = 3, cex =1.1, font = 2, ...)
              points(scene.i.sub[, 'x'], scene.i.sub[, 'y'], pch = pch,cex= 1.4,col = scene.i.sub$cols,  ...)
              points(scene.i[!scene.i[, 'id'] %in% sub, 'x'], scene.i[!scene.i[, 'id'] %in% sub, 'y'], pch = pch, col = scene.i$cols, ...)
            } else {
              points(scene.i[, 'x'], scene.i[, 'y'], pch = pch, col = scene.i$cols, ...)
            }
          }
        }
      } else if (temp & spat){
        xcoord <- 'easting'
        ycoord <- 'northing'
        par(mar = c(1,9,1,1), xpd = T)
        plot.default(scene.i[, 'x'], scene.i[, 'y'], type = "n", ylab = ycoord, xaxt = 'n', xlim = c(emin,emax),ylim = c(nmin,nmax), ...)
        mtext(names(scene)[i],side = 2,adj = 0.5, cex = 0.75, line = 8, font = 2, las = 1)
        if (i == 1){
          title(main = 'spatial and temporal plot', outer = T)
          if (is.null(pch)) {
            text(scene.i[, 'x'], scene.i[, 'y'], scene.i[, 'id'], col = scene.i$cols, ...)
          } else {
            if (!is.null(sub)){
              scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
              text(scene.i.sub[, 'x'], scene.i.sub[, 'y'], scene.i.sub[, 'id'], pos = 3, cex =1.1, font = 2, ...)
              points(scene.i.sub[, 'x'], scene.i.sub[, 'y'], pch = pch,cex= 1.4,col = scene.i.sub$cols,  ...)
              points(scene.i[!scene.i[, 'id'] %in% sub, 'x'], scene.i[!scene.i[, 'id'] %in% sub, 'y'], pch = pch, col = scene.i$cols, ...)
            } else {
              points(scene.i[, 'x'], scene.i[, 'y'], pch = pch, col = scene.i$cols, ...)
            }
          }
        } else if(i == nr){
          mtext('easting',side = 1,adj = 0.5, cex = 0.75, line = 3)
          axis(1)
          if (is.null(pch)) {
            text(scene.i[, 'x'], scene.i[, 'y'], scene.i[, 'id'], col = scene.i$cols, ...)
          } else {
            if (!is.null(sub)){
              scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
              text(scene.i.sub[, 'x'], scene.i.sub[, 'y'], scene.i.sub[, 'id'], pos = 3, cex =1.1, font = 2, ...)
              points(scene.i.sub[, 'x'], scene.i.sub[, 'y'], pch = pch,cex= 1.4,col = scene.i.sub$cols,  ...)
              points(scene.i[!scene.i[, 'id'] %in% sub, 'x'], scene.i[!scene.i[, 'id'] %in% sub, 'y'], pch = pch, col = scene.i$cols, ...)
            } else {
              points(scene.i[, 'x'], scene.i[, 'y'], pch = pch, col = scene.i$cols, ...)
            }
          }
          par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
          plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
          legend('topleft', legend = c(format(opening, format = "%b %d"),' ',' ',' ',
                                       format(attr(scene.i,'origin')+round(mean(c(minstart,maxstart))),format = "%b %d"),'',' ',' ',format(attr(scene.i,'origin')+maxstart, format = "%b %d")),
                 fill = colorRampPalette(c('blue','red'))(9),ncol = 1, bty = 'n',xpd = T, y.intersp = 0.68, title = 'start date', inset = c(0.02,0.03))
        } else {
          if (is.null(pch)) {
            text(scene.i[, 'x'], scene.i[, 'y'], scene.i[, 'id'], col = scene.i$cols, ...)
          } else {
            if (!is.null(sub)){
              scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
              text(scene.i.sub[, 'x'], scene.i.sub[, 'y'], scene.i.sub[, 'id'], pos = 3, cex =1.1, font = 2, ...)
              points(scene.i.sub[, 'x'], scene.i.sub[, 'y'], pch = pch,cex= 1.4,col = scene.i.sub$cols,  ...)
              points(scene.i[!scene.i[, 'id'] %in% sub, 'x'], scene.i[!scene.i[, 'id'] %in% sub, 'y'], pch = pch, col = scene.i$cols, ...)
            } else {
              points(scene.i[, 'x'], scene.i[, 'y'], pch = pch, col = scene.i$cols, ...)
            }
          }
        }

      } else if(spat & comp){
        xcoord <- 'easting'
        ycoord <- 'northing'
        par(mar = c(1,9,1,1))
        plot.default(scene.i[, 'x'], scene.i[, 'y'], type = "n", ylab = ycoord, xaxt = 'n', xlim = c(emin,emax),ylim = c(nmin,nmax), ...)
        mtext(names(scene)[i],side = 2,adj = 0.5, cex = 0.75, line = 8, font = 2, las = 1)
        if (i == 1){
          title(main = 'spatial and mating type plot', outer = T)
          if (is.null(pch)) {
            text(scene.i[, 'x'], scene.i[, 'y'], scene.i[, 'id'], ...)
          } else {
            if (!is.null(sub)){
              scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
              text(scene.i.sub[, 'x'], scene.i.sub[, 'y'], scene.i.sub[, 'id'], pos = 3, cex =1.1, font = 2, col = 'blue', ...)
              points(scene.i.sub[, 'x'], scene.i.sub[, 'y'], pch = pch,cex= 1.4,  ...)
              points(scene.i[!scene.i[, 'id'] %in% sub, 'x'], scene.i[!scene.i[, 'id'] %in% sub, 'y'], pch = pch, ...)
              text (scene.i[,'x'], scene.i[,'y'], paste(scene.i[,'s1'],', ',scene.i[,'s2'], sep = ""), pos = 2, cex = 0.9)
            } else {
              points(scene.i[, 'x'], scene.i[, 'y'], pch = pch, ...)
              text (scene.i[,'x'], scene.i[,'y'], paste(scene.i[,'s1'],', ',scene.i[,'s2'], sep = ""), pos = 2, cex = 0.9)
            }
          }
        } else if(i == nr){
          mtext('easting',side = 1,adj = 0.5, cex = 0.75, line = 3)
          axis(1)
          if (is.null(pch)) {
            text(scene.i[, 'x'], scene.i[, 'y'], scene.i[, 'id'], ...)
          } else {
            if (!is.null(sub)){
              scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
              text(scene.i.sub[, 'x'], scene.i.sub[, 'y'], scene.i.sub[, 'id'], pos = 3, cex =1.1, font = 2, col = 'blue', ...)
              points(scene.i.sub[, 'x'], scene.i.sub[, 'y'], pch = pch,cex= 1.4,  ...)
              points(scene.i[!scene.i[, 'id'] %in% sub, 'x'], scene.i[!scene.i[, 'id'] %in% sub, 'y'], pch = pch, ...)
              text (scene.i[,'x'], scene.i[,'y'], paste(scene.i[,'s1'],', ',scene.i[,'s2'], sep = ""), pos = 2, cex = 0.9)
            } else {
              points(scene.i[, 'x'], scene.i[, 'y'], pch = pch, ...)
              text (scene.i[,'x'], scene.i[,'y'], paste(scene.i[,'s1'],', ',scene.i[,'s2'], sep = ""), pos = 2, cex = 0.9)
            }
          }
        } else {
          if (is.null(pch)) {
            text(scene.i[, 'x'], scene.i[, 'y'], scene.i[, 'id'], ...)
          } else {
            if (!is.null(sub)){
              scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
              text(scene.i.sub[, 'x'], scene.i.sub[, 'y'], scene.i.sub[, 'id'], pos = 3, cex =1.1, font = 2,col = 'blue',...)
              points(scene.i.sub[, 'x'], scene.i.sub[, 'y'], pch = pch,cex= 1.4,  ...)
              points(scene.i[!scene.i[, 'id'] %in% sub, 'x'], scene.i[!scene.i[, 'id'] %in% sub, 'y'], pch = pch, ...)
              text (scene.i[,'x'], scene.i[,'y'], paste(scene.i[,'s1'],', ',scene.i[,'s2'], sep = ""), pos = 2, cex = 0.9)
            } else {
              points(scene.i[, 'x'], scene.i[, 'y'], pch = pch, ...)
              text (scene.i[,'x'], scene.i[,'y'], paste(scene.i[,'s1'],', ',scene.i[,'s2'], sep = ""), pos = 2, cex = 0.9)
            }
          }
        }
      } else if (temp & comp){
        par(xpd = F)
        par(mar = c(1,4,0.5,1))
        scene.i$s1 <- as.numeric(scene.i$s1)
        scene.i$s2 <- as.numeric(scene.i$s2)

        for (j in 1:nrow(scene.i)){
          if (scene.i[j,'s1'] < scene.i[j,'s2']){
            scene.i[j,c('s1','s2')] <- scene.i[j,c('s2','s1')]
          }
        }
        plot.default(jitter(scene.i$s1), jitter(scene.i$s2), col = scene.i$cols,xlim = c(min(scene.i$s2),max(scene.i$s1)), ylim = c(min(scene.i$s2),max(scene.i$s1)), xaxt = 'n', xlab = NULL, ylab = 's2', pch = pch,...)
        mtext(names(scene)[i],side = 2,adj = 0.5, cex = 0.75, line = 5, font = 2)
        abline(v = c(1:10) - 0.5, lty = 'dotted', col = 'lightgray')
        abline(h = c(1:10) - 0.5 , lty = 'dotted', col = 'lightgray')

        if (i == 1){
          axis(2, at = min(scene.i$s2):max(scene.i$s1), labels = min(scene.i$s2):max(scene.i$s1))
          if (!is.null(sub)){
            scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
            title(main = 'temporal and compatibility plot', outer = T)
            text(scene.i.sub[, 's1'], scene.i.sub[, 's2'], scene.i.sub[, 'id'], pos = 3, cex =1, font = 2, ...)
            legend('topleft', legend = c(format(opening, format = "%b %d"),' ',' ',' ',format(attr(scene.i,'origin')+round(mean(c(minstart,maxstart))), format = "%b %d"),' ',' ',' ',format(attr(scene.i,'origin')+maxstart, format = "%b %d")),cex = 1.5, fill = colorRampPalette(c('blue','red'))(9),ncol = 1, bty = 'o',xpd = T, y.intersp = 0.68, title = 'start date', inset = c(0.02,0.03))
          }
        } else if(i == nr){
          axis(1, at = min(scene.i$s2):max(scene.i$s1), labels = min(scene.i$s2):max(scene.i$s1))
          axis(2, at = min(scene.i$s2):max(scene.i$s1), labels = min(scene.i$s2):max(scene.i$s1))
          mtext('s1',side = 1,adj = 0.5, cex = 0.75, line = 3)
          if (!is.null(sub)){
            scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
            text(scene.i.sub[, 's1'], scene.i.sub[, 's2'], scene.i.sub[, 'id'], pos = 3, cex =1, font = 2, ...)
          }
        } else {
          abline(v = c(1:10) - 0.5, lty = 'dotted', col = 'lightgray')
          abline(h = c(1:10) - 0.5 , lty = 'dotted', col = 'lightgray')
          axis(2, at = min(scene.i$s2):max(scene.i$s1), labels = min(scene.i$s2):max(scene.i$s1))
          if (!is.null(sub)){
            scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
            text(scene.i.sub[, 's1'], scene.i.sub[, 's2'], scene.i.sub[, 'id'], pos = 3, cex =1, font = 2, ...)
          }
        }
      }
    }
  } else {

    par(mfrow = c(1,1))
    par(oma = c(1,1,1,1))
    par(mar = c(4,7,0,0))
    par(xpd = T)

    if ("auto" %in% dimension) {
      temp <- attr(scene, "t")
      spat <- attr(scene, "s")
      comp <- attr(scene, "mt")
    } else {
      temp <- F
      spat <- F
      comp <- F
      if ("t" %in% dimension) {
        temp <- T
      }
      if ("s" %in% dimension) {
        spat <- T
      }
      if ("mt" %in% dimension) {
        comp <- T
      }
    }

    if (!is.null(sub)){
      if(sub == 'random'){
        sub <- sample(scene[,'id'],9)
      } else if(sub == 'all'){
        sub <- scene[,'id']
      }
    }

    if(temp){
      scene$cols <- cut(scene$start,9)
      palette(colorRampPalette(c('blue','red'))(9))
      opening <- attr(scene,'origin')+min(scene[,'start'])
      if (attr(scene,'origin')+min(scene[,'start']) < opening){
        opening <- attr(scene,'origin')+min(scene[,'start'])
      }
      closing <- attr(scene,'origin')+max(scene[,'end'])
      if (attr(scene,'origin')+max(scene[,'end']) > closing){
        closing <- attr(scene,'origin')+max(scene[,'end'])
      }

      maxstart <- max(scene[,'start'])
      if (max(scene[,'start']) > maxstart){
        maxstart <-max(scene[,'start'])
      }

      minstart <- min(scene[,'start'])
      if (min(scene[,'start']) < minstart){
        minstart <-min(scene[,'start'])
      }
      vec <- seq(minstart, maxstart, length.out = 9)
    }
    if (temp & spat & comp){
      xcoord <- 'easting'
      ycoord <- 'northing'
      plot.default(scene[, 'x'], scene[, 'y'], type = "n", xlab = xcoord, ylab = ycoord, ...)
      if (is.null(pch)) {
        text(scene[, 'x'], scene[, 'y'], scene[, 'id'], col = scene$cols, ...)
      } else {
        if (!is.null(sub)){
          scene.sub <- scene[scene[, 'id'] %in% sub, ]
          text(scene.sub[, 'x'], scene.sub[, 'y'], scene.sub[, 'id'], pos = 3, cex =1.1, font = 2, ...)
          points(scene.sub[, 'x'], scene.sub[, 'y'], pch = pch,cex= 1.4,col = scene.sub$cols,  ...)
          points(scene[!scene[, 'id'] %in% sub, 'x'], scene[!scene[, 'id'] %in% sub, 'y'], pch = pch, col = scene$cols, ...)
          text (scene[,'x'], scene[,'y'], paste(scene[,'s1'],', ',scene[,'s2'], sep = ""), pos = 2, cex = 0.7)
        } else {
          points(scene[, 'x'], scene[, 'y'], pch = pch, col = scene$cols, ...)
          text (scene[,'x'], scene[,'y'], paste(scene[,'s1'],', ',scene[,'s2'], sep = ""), pos = 2, cex = 0.7)
        }
      }
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
      legend("topleft", legend = c(format(attr(scene,'origin')+min(scene$start), format = "%b %d"),' ',' ',' ',format(attr(scene,'origin')+median(scene$start), format = "%b %d"),' ',' ',' ',format(attr(scene,'origin')+max(scene$start), format = "%b %d")), fill = colorRampPalette(c('blue','red'))(9),ncol = 1, bty = 'n', y.intersp = 0.68, title = 'start date', inset = c(0.02,0.03))

    } else if (temp & spat){
      xcoord <- 'easting'
      ycoord <- 'northing'
      plot.default(scene[, 'x'], scene[, 'y'], type = "n", xlab = xcoord, ylab = ycoord, ...)
      if (is.null(pch)) {
        text(scene[, 'x'], scene[, 'y'], scene[, 'id'], ...)
      } else {
        if (!is.null(sub)){
          scene.sub <- scene[scene[, 'id'] %in% sub, ]
          text(scene.sub[, 'x'], scene.sub[, 'y'], scene.sub[, 'id'], pos = 3, ...)
          points(scene.sub[, 'x'], scene.sub[, 'y'], pch = 19, col = scene.sub$cols,  ...)
          points(scene[!scene[, 'id'] %in% sub, 'x'], scene[!scene[, 'id'] %in% sub, 'y'], pch = 19, col = scene$cols,  ...)
        } else {
          points(scene[, 'x'], scene[, 'y'], pch = pch, col = scene$cols, ...)
        }
      }
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
      legend("topleft", legend = c(format(attr(scene,'origin')+min(scene$start), format = "%b %d"),' ',' ',' ',format(attr(scene,'origin')+median(scene$start), format = "%b %d"),' ',' ',' ',format(attr(scene,'origin')+max(scene$start), format = "%b %d")), fill = colorRampPalette(c('blue','red'))(9),ncol = 1, bty = 'n', y.intersp = 0.68, title = 'start date', inset = c(0.02,0.03))


    } else if(spat & comp){
      par(xpd = T)
      xcoord <- 'easting'
      ycoord <- 'northing'
      plot.default(scene[, 'x'], scene[, 'y'], type = "n", xlab = xcoord, ylab = ycoord, ...)
      if (is.null(pch)) {
        text(scene[, 'x'], scene[, 'y'], scene[, 'id'], ...)
      } else {
        text (scene[,'x'], scene[,'y'], paste(scene[,'s1'],', ',scene[,'s2'], sep = ""), pos = 2, cex = 0.9)
        if (!is.null(sub)){
          scene.sub <- scene[scene[, 'id'] %in% sub, ]
          text(scene.sub[, 'x'], scene.sub[, 'y'], scene.sub[, 'id'], pos = 3, cex =1, col = 'blue', font = 2, ...)
          points(scene.sub[, 'x'], scene.sub[, 'y'], pch = 19,cex= 1.2,  ...)
          points(scene[!scene[, 'id'] %in% sub, 'x'], scene[!scene[, 'id'] %in% sub, 'y'], pch = 19,cex= 1,  ...)
        } else {
          points(scene[, 'x'], scene[, 'y'], pch = pch, ...)
        }
      }

    } else if (temp & comp){
      par(xpd = F)
      par(mar = c(4,4,0,0))
      scene$s1 <- as.numeric(scene$s1)
      scene$s2 <- as.numeric(scene$s2)
      for (i in 1:nrow(scene)){
        if (scene[i,'s1'] < scene[i,'s2']){
          scene[i,c('s1','s2')] <- scene[i,c('s2','s1')]
        }
      }
      plot(jitter(scene$s1), jitter(scene$s2), col = scene$cols,xlim = c(min(scene$s2),max(scene$s1)), ylim = c(min(scene$s2),max(scene$s1)), xlab = 's1', ylab = 's2', pch = pch,...)
      abline(v = c(1:10) - 0.5, lty = 'dotted', col = 'lightgray')
      abline(h = c(1:10) - 0.5 , lty = 'dotted', col = 'lightgray')
      axis(1, at = min(scene$s2):max(scene$s1), labels = min(scene$s2):max(scene$s1))
      axis(2, at = min(scene$s2):max(scene$s1), labels = min(scene$s2):max(scene$s1))
      if (!is.null(sub)){
        scene.sub <- scene[scene[, 'id'] %in% sub, ]
        text(scene.sub[, 's1'], scene.sub[, 's2'], scene.sub[, 'id'], pos = 3, cex =1, font = 2, ...)
      }
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
      plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
      legend("topleft", legend = c(format(attr(scene,'origin')+min(scene$start), format = "%b %d"),' ',' ',' ',format(attr(scene,'origin')+median(scene$start), format = "%b %d"),' ',' ',' ',format(attr(scene,'origin')+max(scene$start), format = "%b %d")), fill = colorRampPalette(c('blue','red'))(9),ncol = 1, bty = 'n', y.intersp = 0.68, title = 'start date', inset = c(0.02,0.03))

    }
    par(mar = nm, mfrow = nmfrow, oma = noma, xpd = F)
  }

}
