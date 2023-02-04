##' dailyOMP generates an OMP object giving the daily outcrossed mating potential of individuals based on k nearest neighbors
##'
##' @title Calculate daily outcrossed mating potential
##' @param scene a matingScene object
##' @param k integer, number of nearest neighbors to use in calculating OMP
##' @param days the day or range of days to calculate OMP for, default is all days in a scene (see Details)
##' @param gamma parameter of exponential decay to be used in calculating OMP (defaults to 1/13)
##' @param nn.constant logical; indicates whether the nearest neighbors used in calculations should be the nearest on a given day (\code{nn.constant = FALSE}) or the nearest neighbors over an entire season (\code{nn.constant = TRUE})
##' @param sum logical; indicates if the return should be a sum of an individual's daily outcrossed mating potential over the range specified by days
##' @param mean logical; indicates if the return should be the mean of an individual's daily outcrossed mating potential over the range of days that the individual was receptive to mating
##' @return a named matrix with a row for each id and a column for each day, and entries corresponding to ids' OMP each day
##' @details Daily outcrossed mating potential is a weighted average of an individual's distance to their nearest neighbors on a given day (Wagenius et al. 2007). The days to calculate OMP for should be input as integers relative to the first day of flowering, as they are in the start and end columns of a matingScene object. If the number of ids receptive on a day is less than k, OMP will be calculated for the maximum number of neighbors.
##' @references Wagenius, S., E. Lonsdorf, and C. Neuhauser. 2007. Patch aging and the S-Allee effect: breeding system effects on the demographic response of plants to habitat fragmentation. \emph{American Naturalist} \strong{169}:383-397.
##' @seealso \code{\link{makeScene}}, \code{\link{proximity}}, \code{\link{synchrony}}, \code{\link{receptivityByDay}}
##' @author Amy Waananen
##' @examples
##' pop <- simulateScene()
##' omp <- dailyOMP(pop)
##' omp.1 <- dailyOMP(pop, nn.constant = TRUE) # same nearest neighbors throughout the season
##' omp.2 <- dailyOMP(pop, nn.constant = FALSE) # nearest flowering neighbors


dailyOMP <- function(scene, k = 3, days = NULL, gamma = 1/13.3, nn.constant = FALSE, sum = FALSE, mean = FALSE){
  if(is.list(scene) & !is.data.frame(scene)){
    out <- lapply(scene, dailyOMP, k = k, days = days, gamma = gamma, sum = sum, mean = mean)
  } else {
    if(is.null(days)){
      days <- min(scene$start):max(scene$end)
    } else{
      days <- days
    }
    out <- matrix(nrow  = nrow(scene), ncol = length(days), dimnames = list(scene$id, as.character(days + attr(scene, 'origin'))))
    rbd <- receptivityByDay(scene)
    if(!nn.constant){
      for (i in 1:length(days)){
        day <- days[i]
        idsDi <- which(rbd[,day])
        nbrs <- k
        if(length(idsDi) <= k) nbrs <- length(idsDi)-1
        if(nbrs > 1){
          knn <- unlist(get.knn(scene[idsDi,c('x','y')], k = nbrs))
          dim(knn) <- c(length(idsDi), nbrs, 2)
          out[idsDi,i] <- rowSums(exp(knn[,,2, drop = FALSE]*-1*gamma))
        }
      }
      out[is.na(out)]<- 0
    } else {
      knn <- unlist(get.knn(scene[,c('x','y')], k = k))
      dim(knn) <- c(nrow(scene), k, 2)
      mat <- matrix(nrow = nrow(scene),ncol = nrow(scene))
      for (y in 1:nrow(scene)){
        mat[y,knn[y,,1]]<-exp(knn[y,,2, drop = FALSE]*-1*gamma)
      }
      mat[is.na(mat)]<- 0
      out <- (mat %*% rbd)*rbd
    }

    if(sum){
      out <- apply(out,1,sum)
      out <- data.frame(id = names(out), omp = out, row.names = 1:length(out))
    } else if(mean){
      out <- apply(out,1,sum)
      rbd <- apply(rbd,1,sum)
      out <- mapply(out,rbd, FUN = function(x,y)x/y)
      out <- data.frame(id = names(out), omp = out, row.names = 1:length(out))
    }
  }
  out
}


