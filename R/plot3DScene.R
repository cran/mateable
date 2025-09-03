##' Visualize multiple dimensions of a mating scene
##'
##' @title multi-dimensional visualization of mating scene object
##' @param scene a matingScene object
##' @param dimension what dimension(s) of the mating scene should be visualized. Possible dimensions are 't' for temporal, 's' for spatial, 'mt' for mating type, and 'auto' (the default). For dimension = 'auto', all dimensions represented in the mating scene object will be plotted.
##' @param sub a subset of the population to plot; either a character indicating whether to subset a random sample (\code{sub}='random'), all individuals (\code{sub}='all'), or a vector containing the IDs of the individuals to subset.
##' @param N if \code{sub} = 'random', the number of individuals to sample (default N = 3)
##' @param xcoord x-axis coordinate system label
##' @param ycoord y-axis coordinate system label
##' @param pch point type, defaults to pch = 19, solid filled in circle. If pch = NULL, individuals will be labeled by their id.
##' @param pt.cex specify point expansion factor (point size relative to device default)
##' @param label.cex specify text expansion factor (text size relative to device default)
##' @param mt1 label for mating type '1', if dioecious; defaults to 'F'
##' @param mt2 label for mating type '2', if dioecious; defaults to 'M'
##' @param plot.lim.zoom if TRUE, spatial plot limits for lists of scenes are set by the maximum from all scenes
##' @param ... optional arguments for the plot function
##' @return No return value, called to draw a plot
##' @export
##' @author Amy Waananen
##' @seealso see generic function \code{graphics::\link[graphics]{points}()} for values of \code{pch}
##' @examples
##' pop <- simulateScene()
##' plot3DScene(pop)
##'
##'
##'
plot3DScene <- function(scene, dimension = "auto",
                        sub= NULL, N = 3,
                        ycoord = 'northing', xcoord = 'easting',
                        pch = 19, pt.cex = 0.7,label.cex = 0.7, mt1 = 'F', mt2 = 'M',
                        plot.lim.zoom = FALSE, ...){
  dimension <- match.arg(dimension, c("auto", "t", "s", "mt"),several.ok = TRUE)
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  if (!is.list(scene[[1]])){
    scene <- list(scene)
  }

  if ("auto" %in% dimension) {
    temp <- attr(scene[[1]], "t")
    spat <- attr(scene[[1]], "s")
    comp <- attr(scene[[1]], "mt")
  } else {
    temp <- F
    spat <- F
    comp <- F
    if ("t" %in% dimension) {temp <- T}
    if ("s" %in% dimension) {spat <- T}
    if ("mt" %in% dimension) {comp <- T}
  }

  if (!is.null(sub)){
    if('random' %in% sub){
      sub <- sample(unique(unlist(sapply(scene,function(x)x[,'id'], simplify = TRUE), use.names = FALSE)),N)
    } else if('all' %in% sub){
      sub <-unique(unlist(sapply(scene,function(x)x[,'id'], simplify = TRUE), use.names = FALSE))
    }
  }

  nr <- length(scene)

  if (nr > 1 ){
    par(mfrow = c(nr,1), xpd = TRUE)
  }

  if(spat){
    emin <- min(unlist(lapply(scene, function(x) x['x'])))
    emax <- max(unlist(lapply(scene, function(x) x['x'])))
    nmin <- min(unlist(lapply(scene, function(x) x['y'])))
    nmax <- max(unlist(lapply(scene, function(x) x['y'])))
  }

  if(temp){
    starts <- unlist(lapply(scene, function(x) x[,'start']+ attr(x,'origin')))
    years <- as.numeric(sapply(scene, function(x) strsplit(as.character(attr(x,'origin')), split = '-')[[1]][1]))
    minstart <- min(strptime(as.Date(starts, origin = '1970-01-01'),format = '%Y-%m-%d')$yday)
    maxstart <- max(strptime(as.Date(starts, origin = '1970-01-01'),format = '%Y-%m-%d')$yday)
    ends <- unlist(lapply(scene, function(x) as.Date(max(x[,'end'] + attr(x,'origin')), origin = '1970-01-01', format = '%j')))
    maxend <- max(strptime(as.Date(ends, origin = '1970-01-01'),format = '%Y-%m-%d')$yday)
    count <- max(unlist(lapply(scene, nrow)))
  }

  if(comp){
    if (length(unique(unlist(lapply(scene, function(x) as.numeric(unlist(x[,c('s1','s2')]))))))==2){
      dioecious <- TRUE
      smin <- 1
      smax <- 2
    }else {
      dioecious <- FALSE
      smin <- min(unlist(lapply(scene, function(x) as.numeric(unlist(x[,c('s1','s2')])))))
      smax <- max(unlist(lapply(scene, function(x) as.numeric(unlist(x[,c('s1','s2')])))))
    }
  }

  for (i in 1:length(scene)){
    scene.i <- scene[[i]]
    if (temp){
      palette(colorRampPalette(c('blue','red'))(9))
      vec <- seq(minstart, maxstart, length.out = 9)
      scene.i$yday <- strptime(scene.i$start + attr(scene.i, "origin"),format = '%Y-%m-%d')$yday
      scene.i$cols <- findInterval(scene.i$yday,vec)
    }
    if(spat){
      if(plot.lim.zoom){
        plot.default(scene.i[, 'x'], scene.i[, 'y'], type = "n", ylab = "",xlab = '', asp = 1, cex = pt.cex, ...)
      } else {
        plot.default(scene.i[, 'x'], scene.i[, 'y'], type = "n", xaxt = 'n', xlim = c(emin,emax),ylim = c(nmin,nmax), xlab = '', ylab = "", asp = 1, cex = pt.cex, ...)
      }
      mtext(ycoord, side = 2, cex = 0.75, adj = 0.5, line = 3)
      mtext(names(scene)[i],side = 2, cex = 0.75, font = 2, las = 1, adj = 0, line = 8)
    }
    if(comp){
      if(dioecious){
        labs <- ifelse(scene.i[,'s1']==1,mt1,mt2)
      } else{
        labs <- paste(scene.i[,'s1'],', ',scene.i[,'s2'], sep = "")
      }
    }
    if (temp & spat & comp){
      if (is.null(pch)) {
        text(scene.i[, 'x'], scene.i[, 'y'], scene.i[, 'id'], col = scene.i$cols, cex = label.cex, ...)
      } else {
        text (scene.i[,'x'], scene.i[,'y'], labs, pos = 2, cex = label.cex)
        if (!is.null(sub)){
          scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
          text(scene.i.sub[, 'x'], scene.i.sub[, 'y'], scene.i.sub[, 'id'], pos = 3, cex =label.cex*1.2, font = 2, ...)
          points(scene.i.sub[, 'x'], scene.i.sub[, 'y'], pch = pch,col = scene.i.sub$cols,cex = pt.cex*1.2,  ...)
          points(scene.i[!scene.i[, 'id'] %in% sub, 'x'], scene.i[!scene.i[, 'id'] %in% sub, 'y'], pch = pch, col = scene.i$cols, cex = pt.cex, ...)
        } else {
          points(scene.i[, 'x'], scene.i[, 'y'], pch = pch, col = scene.i$cols, cex = pt.cex, ...)
        }
      }
      if(i == nr){
        if(!plot.lim.zoom){
          axis(1, ...)
        }
        mtext(xcoord,side = 1,adj = 0.5, cex = 0.75, line = 2)
        par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
        plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
        legend('topleft', legend = c(format(as.Date(minstart, origin = '1970-01-01'), format = "%b %d"),' ',' ',' ', format(as.Date(round(mean(c(minstart,maxstart))), origin = '1970-01-01'),format = "%b %d"),'',' ',' ',format(as.Date(maxstart,origin = '1970-01-01'), format = "%b %d")),fill = colorRampPalette(c('blue','red'))(9),ncol = 1, bty = 'n',xpd = TRUE, y.intersp = 0.68, title = 'start date', cex = 0.75)
      }
    } else if (temp & spat){
      if (is.null(pch)) {
        text(scene.i[, 'x'], scene.i[, 'y'], scene.i[, 'id'], col = scene.i$cols, ...)
      } else {
        if (!is.null(sub)){
          scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
          text(scene.i.sub[, 'x'], scene.i.sub[, 'y'], scene.i.sub[, 'id'], pos = 3, cex =label.cex, font = 2, ...)
          points(scene.i.sub[, 'x'], scene.i.sub[, 'y'], pch = pch,col = scene.i.sub$cols,cex = pt.cex*1.2,  ...)
          points(scene.i[!scene.i[, 'id'] %in% sub, 'x'], scene.i[!scene.i[, 'id'] %in% sub, 'y'], pch = pch, cex = pt.cex, col = scene.i$cols, ...)
        } else {
          points(scene.i[, 'x'], scene.i[, 'y'], pch = pch, col = scene.i$cols, cex = pt.cex, ...)
        }
      }
      if(i == nr){
        mtext(xcoord,side = 1,adj = 0.5, cex = 0.75, line = 3)
        if(!plot.lim.zoom){
          axis(1, ...)
        }
        par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
        plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
        # legend('topleft', legend = c(format(attr(scene.i,'origin')+minstart, format = "%b %d"),' ',' ',' ', format(attr(scene.i,'origin')+round(mean(c(minstart,maxstart))),format = "%b %d"),'',' ',' ',format(attr(scene.i,'origin')+maxstart, format = "%b %d")),fill = colorRampPalette(c('blue','red'))(9),ncol = 1, bty = 'n',xpd = TRUE, y.intersp = 0.68, title = 'start date', cex = 0.75)
        legend('topleft', legend = c(format(as.Date(minstart, origin = '1970-01-01'), format = "%b %d"),' ',' ',' ', format(as.Date(round(mean(c(minstart,maxstart))), origin = '1970-01-01'),format = "%b %d"),'',' ',' ',format(as.Date(maxstart,origin = '1970-01-01'), format = "%b %d")),fill = colorRampPalette(c('blue','red'))(9),ncol = 1, bty = 'n',xpd = TRUE, y.intersp = 0.68, title = 'start date', cex = 0.75)

      }
    } else if(spat & comp){
      if (is.null(pch)) {
        text(scene.i[, 'x'], scene.i[, 'y'], scene.i[, 'id'], cex = label.cex, ...)
      } else {
        if (!is.null(sub)){
          scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
          text(scene.i.sub[, 'x'], scene.i.sub[, 'y'], scene.i.sub[, 'id'], pos = 3, cex =label.cex*1.2, font = 2, col = 'blue', ...)
          points(scene.i.sub[, 'x'], scene.i.sub[, 'y'], pch = pch, cex = pt.cex*1.2,  ...)
          points(scene.i[!scene.i[, 'id'] %in% sub, 'x'], scene.i[!scene.i[, 'id'] %in% sub, 'y'], pch = pch, cex = pt.cex*1.2, ...)
          text (scene.i[,'x'], scene.i[,'y'], labs, pos = 2, cex = label.cex)
        } else {
          points(scene.i[, 'x'], scene.i[, 'y'], pch = pch, cex = pt.cex, ...)
          text (scene.i[,'x'], scene.i[,'y'], labs, pos = 2, cex = label.cex)
        }
      }
      if(i == nr){

        mtext(xcoord,side = 1,adj = 0.5, cex = 0.75, line = 3)
        if(!plot.lim.zoom){
          axis(1, ...)
        }
      }
    } else if (temp & comp){
      if (dioecious){
        scene.i <- scene.i[order(scene.i[, 'start'], scene.i[, 'end']),]
        scene.i$index <- seq_along(scene.i[, 1])
        plot.default(scene.i[, 'start'], scene.i$index, ylim = c(1,count), xlim = c(minstart, maxend), type = "n", xlab = 'date', ylab = "",xaxt = 'n',yaxt = 'n', ...)
        segments(scene.i[, 'start'], scene.i$index, scene.i[, 'end'],scene.i$index, col = "gray50", cex = 3, ...)
        text(scene.i[, 'start']-0.02*maxstart, scene.i[, 'index'], labs, cex = label.cex)
        mtext('count',side = 2,adj = 0.5, cex = 0.75, line = 2.5)
        axis(2,...)
        if (i == nr){
          datLabs <- seq(minstart,maxend, by = 7)
          axis(1, at = datLabs, labels = format(as.Date(datLabs, origin = as.Date("1970-01-01")),format = "%b %d"), tick=0.25, ...)
          mtext('date',side = 1,adj = 0.5, cex = 0.75, line = 3)
        }
      }else {
        par(mar = c(1,3,0,1),xpd = FALSE)
        scene.i$s1 <- as.numeric(scene.i$s1)
        scene.i$s2 <- as.numeric(scene.i$s2)
        for (j in 1:nrow(scene.i)){
          if (scene.i[j,'s1'] < scene.i[j,'s2']){
            scene.i[j,c('s1','s2')] <- scene.i[j,c('s2','s1')]
          }
        }
        plot(jitter(scene.i[,'s1']), jitter(scene.i[,'s2']), col = scene.i$cols,xlim = c(min(scene.i$s2),max(scene.i$s1)), ylim = c(min(scene.i$s2),max(scene.i$s1)), xaxt = 'n',yaxt = 'n', xlab = '', ylab = '', pch = pch)
        mtext(names(scene)[i],side = 2,cex = 0.75, line = 4.5, font = 2, las = 1)
        mtext('s2', side = 2, outer = TRUE, cex = 0.7)
        abline(v = c(1:10) - 0.5, lty = 'dotted', col = 'lightgray')
        abline(h = c(1:10) - 0.5 , lty = 'dotted', col = 'lightgray')
        axis(2, at = smin:smax, labels = min(scene.i$s2):max(scene.i$s1), cex.axis = 0.75)
        if (!is.null(sub)){
          scene.i.sub <- scene.i[scene.i[, 'id'] %in% sub, ]
          text(scene.i.sub[, 's1'], scene.i.sub[, 's2'], scene.i.sub[, 'id'], pos = 3, cex =1, font = 2, ...)
        }
        if (i == 1){
          # legend('topleft', legend = c(format(attr(scene.i,'origin')+ minstart, format = "%b %d"),' ',' ',' ',format(attr(scene.i,'origin')+round(mean(c(minstart,maxstart))), format = "%b %d"),' ',' ',' ',format(attr(scene.i,'origin')+maxstart, format = "%b %d")), fill = colorRampPalette(c('blue','red'))(9),ncol = 1, bty = 'o',xpd = TRUE, y.intersp = 0.68, title = 'start date', inset = c(0.02,0.03), cex = 0.75)
          legend('topleft', legend = c(format(minstart, format = "%b %d"),' ',' ',' ',format(round(mean(c(minstart,maxstart))), format = "%b %d"),' ',' ',' ',format(maxstart, format = "%b %d")), fill = colorRampPalette(c('blue','red'))(9),ncol = 1, bty = 'o',xpd = TRUE, y.intersp = 0.68, title = 'start date', inset = c(0.02,0.03), cex = 0.75)

        }
        if(i == nr){
          axis(1, at = min(scene.i$s2):max(scene.i$s1), labels = min(scene.i$s2):max(scene.i$s1), ...)
          mtext('s1',side = 1,adj = 0.5, cex = 0.75, line = 3)
        }
      }
    }
  }
}
