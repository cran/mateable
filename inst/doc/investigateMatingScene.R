## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(fig.width=6, fig.height=4)

## -----------------------------------------------------------------------------
library(mateable)
packageDescription("mateable")$Version

oldpar <- par(no.readonly = TRUE)


## -----------------------------------------------------------------------------
str(ech2012)

## -----------------------------------------------------------------------------
eelr <- ech2012[ech2012$pop %in% 'eelr',]
eelr <- makeScene(eelr, startCol = "firstDay", endCol = "lastDay",
                       xCol = "Ecoord", yCol = "Ncoord", idCol = "tagNo")


## -----------------------------------------------------------------------------
focalPlants <- c(17217, 17202, 14582, 15114, 7614, 1509, 17002, 7431, 3370)
par(mar = c(3,4,1,1), oma = c(2,0,0,0))
plotScene(eelr, c('s','t'), sub = focalPlants, N = 4, label.cex = 0.5, plot.lim.zoom = TRUE)

## ----collapse = TRUE----------------------------------------------------------
eSum <- matingSummary(eelr)

## -----------------------------------------------------------------------------
# make scene based off eelr summary information
simScene <- simulateScene(size = nrow(eelr), 
                          meanSD = eSum$meanSD, # mean start date
                          sdSD = eSum$sdSD,  # standard deviation of start date
                          meanDur = eSum$meanDur, # mean duration of reproductive bout
                          sdDur = eSum$sdDur, # standard deviation of duration of reproductive bout
                          xRange = c(eSum$minX, eSum$maxX), # range of spatial x-coordinates
                          yRange = c(eSum$minY, eSum$maxY)) # range of spatial y-coordinates

## -----------------------------------------------------------------------------
eProx <- proximity(eelr, "maxPropSqrd")
eProx$pop
hist(eProx$ind$proximity, 30)


## -----------------------------------------------------------------------------
par(mfrow = c(1,3), oma = c(1,1,1,2), mar = c(4,3,1,3))
plotPotential(eProx)

## -----------------------------------------------------------------------------
par(mfrow = c(1,1))
plotPotential(eProx, plotType = "net", sub.ids = focalPlants)

## -----------------------------------------------------------------------------
eOver <- overlap(eelr, compareToSelf = TRUE) # matrix of days overlapping
hist(eOver, 40, main = "Histogram of days overlapping between pair") 

## ----fig.show = 'hold'--------------------------------------------------------
eRecep <- receptivityByDay(eelr) # T/F receptive on each day
str(eRecep) # matrix
dailyReceptivitySummary <- receptivityByDay(eelr, summary = TRUE)
dailyReceptivitySummary # a named integer vector
plot(as.Date(names(dailyReceptivitySummary)), dailyReceptivitySummary,
     xlab = 'date', ylab = 'count of receptive individuals')


## -----------------------------------------------------------------------------
eSync <- synchrony(eelr, "overlap")
hist(eSync$ind[, 2], 30)
abline(v = eSync$pop, col ="red", lwd = 2)
abline(v = synchrony(eelr, "overlap", averageType = "median")$pop,
       col = "blue", lwd = 2)

## -----------------------------------------------------------------------------
par(mfrow = c(1,3), mar = c(2,4,2,4), oma = c(4,4,4,4))
plotPotential(eSync, sub.ids = focalPlants)

## -----------------------------------------------------------------------------
par(mar = c(4,4,1,1))
plot3DPotential(list(eSync,eProx), sub.ids = focalPlants)

## -----------------------------------------------------------------------------
sComp <- compatibility(simScene, "si_echinacea")
par(mfrow = c(1,3))
plotPotential(sComp, density = FALSE)
par(mfrow = c(1,2))
plotPotential(sComp, plotType = c('net','heat'), density = FALSE)

## -----------------------------------------------------------------------------
ech2012a <- makeScene(ech2012, startCol = "firstDay", endCol = "lastDay",
                       xCol = "eCoord", yCol = "nCoord", idCol = "tag",
                       split = "pop")

par(mar = c(3,4,1,1), oma = c(2,0,0,0))
plotScene(ech2012a, plot.lim.zoom = TRUE) # spatial plot limits set by range of coordinates in each scene


## -----------------------------------------------------------------------------
ech2012b <- makeScene(ech2012, startCol = "firstDay", endCol = "lastDay",
                       xCol = "eCoord", yCol = "nCoord", idCol = "tag",
                       otherCols = "pop")

par(mar = c(3,4,1,1), oma = c(2,0,0,0))
plotScene(ech2012b, colorBy = 'pop')
plotScene(ech2012b, colorBy = 'pop', sortBy = c('pop','start','end')) # specify the stacking order of segments in the mating schedule using sortBy, listing the column names to sort by in descending levels


## -----------------------------------------------------------------------------
simScene1 <- simulateScene(size = nrow(eelr), meanSD = eSum$meanSD,
                          sdSD = eSum$sdSD, meanDur = eSum$meanDur,
                          sdDur = eSum$sdDur, xRange = c(eSum$minX, eSum$maxX),
                          yRange = c(eSum$minY, eSum$maxY))
simScene2<- simulateScene(size = 1.5*nrow(eelr), meanSD = eSum$meanSD + 365,
                          sdSD = eSum$sdSD, meanDur = eSum$meanDur,
                          sdDur = eSum$sdDur, xRange = c(eSum$minX, eSum$maxX),
                          yRange = c(eSum$minY, eSum$maxY))
simScene3 <- simulateScene(size = 0.8*nrow(eelr), meanSD = eSum$meanSD + 730,
                          sdSD = eSum$sdSD, meanDur = eSum$meanDur,
                          sdDur = eSum$sdDur, xRange = c(eSum$minX, eSum$maxX),
                          yRange = c(eSum$minY, eSum$maxY))

multiYearScene <- list('2012' = simScene1,'2013' = simScene2, '2014' = simScene3)

## ----fig.height = 8-----------------------------------------------------------
plotScene(multiYearScene,sub = c(1,6,12,18,13,24,55,45,60), label.cex = 0.8)

## ----fig.height = 8-----------------------------------------------------------
par(mfrow = c(3,1), mar = c(1,5,1,1), oma = c(4,4,4,0))
plot3DScene(multiYearScene, pt.cex = 1.2, sub = c(1,6,12,18,13,24,55,45,60))

## -----------------------------------------------------------------------------
syncMulti <- synchrony(multiYearScene, method = 'augs')
proxMulti <- proximity(multiYearScene, method = 'maxPropSqrd')
compatMulti <- compatibility(multiYearScene, method = 'si_echinacea')

str(syncMulti) # a list of lists

## ----fig.height = 8-----------------------------------------------------------
par(mfrow = c(3,3))
plotPotential(syncMulti, sub.ids = c(1,6,12,18,13,24,55,44,60))

## ----fig.height = 8-----------------------------------------------------------
par(mfrow = c(3,1), mar = c(1,5,1,1), oma = c(4,4,4,0))
plot3DPotential(list(syncMulti, proxMulti, compatMulti), subject = 'ind', pt.cex = 1, sub.ids = c(1,6,12,18,13,24,55,45,60))

## -----------------------------------------------------------------------------
par(oldpar)

