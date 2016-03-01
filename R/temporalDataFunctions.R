##' Summarize a Mating Scene
##'
##' Create a summary of information contained within a matingScene object.
##'
##' @param scene a matingScene object
##' @param type character. whether to do a temporal (t), spatial (s), or mating
##' type (mt) summary. The default is "auto" which will automatically summarize
##' all mating information in scene
##' @param k integer. Which nearest neighbor to calculate (only for type == "s")
##' @param compatMethod character indicating the method to use when calculating
##' compatiblity. Defaults to "si_echinacea"
##' @return a list or a list of lists containing summary information
##' including:\cr
##' temporal - year (year), population start date (popSt), mean individual start date
##' (meanSD), standard deviation of start (sdSD), mean duration (meanDur),
##' standard deviation of duration (sdDur), peakDay - day(s) on which highest
##' number of individuals were receptive (peak), mean end date (meanED),
##' standard deviation of end date (sdED), population end date (popEnd)\cr
##' spatial - minimum x (minX), minimum y (minY), maximum x (maxX),
##' maximum y (maxY), average distance to kth nearest neighbor as specified
##' by k (k<n> where n is the input for k)\cr
##' compatibility - number of mating types (nMatType), average number of
##' compatible mates (meanComp)\cr
##' If scene is a multi-year matingScene, then the output will be a list
##' of lists, one list for each year.
##' @examples
##' eelr <- makeScene(eelr2012, startCol = "firstDay", endCol = "lastDay",
##'   xCol = "Ecoord", yCol = "Ncoord", idCol = "tagNo")
##' eelrSum <- matingSummary(eelr)
##' eelrSum[c("minX", "minY", "maxX", "maxY")]
matingSummary <- function(scene, type = "auto", k = 1,
                          compatMethod = "si_echinacea") {
  if (is.list(scene) & !is.data.frame(scene)) {
    matSum <- lapply(scene, matingSummary)
  } else {
    type <- match.arg(type, c("auto", "t", "s", "mt"))
    matSum <- list()
    if (type == "auto") {
      temp <- attr(scene, "t")
      spat <- attr(scene, "s")
      comp <- attr(scene, "mt")
    } else {
      temp <- F
      spat <- F
      comp <- F
      if (type == "t") {
        temp <- T
      } else if (type == "s") {
        spat <- T
      } else if (type == "mt") {
        comp <- T
      }
    }
    if (temp) {
      org <- attr(scene, "origin")
      matSum$year <- as.numeric(format(org, "%Y"))
      matSum$popSt <- as.Date(1, org)
      matSum$meanSD <- as.Date(mean(scene$start), org)
      matSum$sdSD <- sd(scene$start)
      matSum$meanDur <- mean(scene$duration)
      matSum$sdDur <- sd(scene$duration)
      matSum$peak <-
        as.Date(as.integer(which.max(colSums(receptivityByDay(scene)))), org)
      matSum$meanED <- as.Date(mean(scene$end), org)
      matSum$sdED <- sd(scene$end)
      matSum$popEnd <- as.Date(max(scene$end), org)
    }
    if (spat) {
      matSum$minX <- min(scene$x)
      matSum$minY <- min(scene$y)
      matSum$maxX <- max(scene$x)
      matSum$maxY <- max(scene$y)
      matSum[[paste("k", k, sep = "")]] <-
        mean(kNearNeighbors(scene, k)[k])
    }
    if (comp) {
      matSum$nMatType <- length(union(levels(scene$s1), levels(scene$s2)))

      matSum$meanComp <- compatibility(scene, compatMethod)$pop * 100
    }
  }
  matSum
}

##' Pairwise Mating Timing Comparison
##'
##' Get comparisons of mating timing between all pairs
##'
##' @param scene a matingScene object
##' @param overlapOrTotal whether to calculate the number of days that each
##' pair was overlapping in mating receptivity or the total number of days
##' that either individual was receptive
##' @param compareToSelf whether or not to include self comparisons in the
##' return value
##' @return a matrix containing all pairwise comparisons. If compareToSelf
##' is FALSE then there will be n rows and n-1 columns. \cr
##' To index result[i,j] where j > i, use \code{result[i,j-1]}, where \code{result}
##' is the return value of overlap. There is one attribute "idOrder"
##' which holds the order of the id column in scene at the time of the function call.
##' This can be useful to find certain elements in the matrix (see examples). \cr
##' If scene is a multi-year matingScene, then overlap will return a list of matrices
##' (as described above) where each matrix represents one year.
##' @author Danny Hanson
##' @examples
##' pop <- simulateScene()
##' pop <- pop[order(pop$start),]
##' daysSync <- overlap(pop)
##' indices <- which(attr(daysSync, "idOrder") %in% c(1, 4))
##' if (indices[1] <= indices[2]) {
##'   daysSync[indices[1], indices[2]]
##' } else {
##'   daysSync[indices[1], indices[2]-1]
##' }
overlap <- function(scene, overlapOrTotal = c("overlap", "total"),
                    compareToSelf = FALSE) {
  if (is.list(scene) & !is.data.frame(scene)) {
    overlapMatrix <- lapply(scene, overlap)
  } else {
    n <- nrow(scene)
    overlapOrTotal <- match.arg(overlapOrTotal)

    if (overlapOrTotal == "overlap") {
      if (compareToSelf) {
        overlapMatrix <- daysSync_self(scene$start, scene$end, n)
      } else {
        overlapMatrix <- daysSync_noself(scene$start, scene$end, n)
      }
    } else if (overlapOrTotal == "total") {
      if (compareToSelf) {
        overlapMatrix <- daysEither_self(scene$start, scene$end, n)
      } else {
        overlapMatrix <- daysEither_noself(scene$start, scene$end, n)
      }
    }
    attr(overlapMatrix, "idOrder") <- scene$id
  }
  overlapMatrix
}


##' Mating Receptivity by Day
##'
##' Create a matrix showing which individuals are receptive on a given day.
##'
##' @param scene a matingScene object
##' @return a matrix where the columns represent all mating days and the rows
##' represent all individuals in the population. The value at position
##' [i,j] will be TRUE if individual j was receptive on day i \cr
##' If scene is a multi-year matingScene, then overlap will return a list of matrices
##' (as described above) where each matrix represents one year.
##' @author Danny Hanson
##' @examples
##' pop <- simulateScene(size = 10)
##' receptivityByDay(pop)
receptivityByDay <- function(scene) {
  if (is.list(scene) & !is.data.frame(scene)) {
    dailyMatrix <- lapply(scene, receptivityByDay)
  } else {
    # get ids and days that flowering occurred
    ids <- scene$id
    days <- seq(min(scene$start), max(scene$end), 1)
    nID <- length(ids)
    nDay <- length(days)

    dailyMatrix <- matrix(F, nrow = nID, ncol = nDay)
    # for a given individual, say what days it was flowering on
    for (i in 1:nID) {
      dailyMatrix[i, scene[i, "start"]:scene[i, "end"]] <- T
    }

    rownames(dailyMatrix) <- ids
    colnames(dailyMatrix) <- days
  }
  dailyMatrix
}

##' Calculate one of a variety of measures of mating synchrony.
##'
##' @title Make potentials object--mating synchrony
##' @param scene a matingScene object that includes the flowering schedule for the
##' scene of interest.
##' @param method character, partial matching allowed, describing what type
##' of synchrony will be calculated. "augspurger" is based on the method
##' described in Augspurger (1983). "kempenaers" is based on the method
##' described in Kempenaers (1993). "sync_prop" will calculate individual
##'  synchrony based on the proportion of the sum of all individuals' days
##'  available to mate that coincided with the individual's days available for mating.
##' "overlap" will calculate a synchrony value based on the number of days both
##' individuals were flowering divided by the number of days either individual
##' was available for mating. "sync_nn" gives the average of the kth nearest
##' neighbor, or rather the kth most synchronous individual.
##' @param subject one of "population", "pairwise", "individual", or "all"
##' - see Value for more details.
##' @param averageType character. Identifies whether to take the mean or median
##' when calculating averages
##' @param syncNN integer. The kth nearest neighbor to be averaged when
##' calculating population synchrony
##' @param compareToSelf logical. Whether or not to include self comparisons
##' when calculation synchrony. Defaults to FALSE.
##' @return A potentials object containing one more more of the following, depending the
##' input for \code{subject}: \cr
##' If \code{subject} is "population" \code{synchrony} will return a numeric
##' value that has a range depending on the \code{method}. If
##' \code{subject} is "pairwise" \code{synchrony} will return a matrix
##' with all pairwise synchrony comparisons. It is important to note two things:
##' [1] if \code{method} is set to "sync_nn" then the pairwise comparisons will
##' be in descending order and cannot be indexed by ID order. [2] if
##' \code{compareToSelf} is set to FALSE, the matrix will have dimensions 100
##' rows by 99 columns. Similar to \code{\link{overlap}}, indexing will be
##' affected. If \code{subject} is "individual" \code{synchrony} will
##' returns a data frame with a row for id and a row for individual synchrony.
##' If \code{subject} is "all" \code{synchrony} will return a list
##' containing the values described above for population, pairwise, and
##' individual synchrony.
##' @details Measures of synchrony are based on methods described in Augspurger (1983),
##' Kempenaers (1983), and from Ison and Wagenius (2014), as well
##' as variations on different factors of those measures.
##' @export
##' @author Danny Hanson
##' @references Kempenaers, B. (1993) The use of a breeding synchrony index.
##' \emph{Ornis Scandinavica}, \strong{24}, 1. \cr\cr
##' Augspurger, C.K. (1983) Phenology, flowering synchrony, and fruit set of
##' six neotropical shrubs. \emph{Biotropica} \strong{15}, 257-267.
##' @examples
##' pop <- simulateScene(size = 150)
##' synchrony(pop, "augs")
##'
##' pop2 <- simulateScene(size = 1234, sdDur = 5, sk = 1)
##' syncVals <- synchrony(pop2, "sync_nn", "all", "median", 123)
synchrony <- function(scene, method, subject = "all", averageType = "mean",
                      syncNN = 1, compareToSelf = FALSE) {

  method <- match.arg(method, c("augspurger", "kempenaers", "sync_prop",
                                "overlap", "sync_nn"))
  subject <- match.arg(subject, c("population", "pairwise",
                                  "individual", "all"),
                       several.ok = T)
  averageType <- match.arg(averageType, c("mean", "median"))

  # some things that all methods need
  n <- nrow(scene) # population size
  if (averageType == "mean") {
    average <- mean
  } else if (averageType == "median") {
    average <- median
  }
  # deal with pop size and transposing matrices
  if (n < 2) {
    stop("Can't calculate synchrony for population size less than 2")
  }

  if (method == "augspurger") {
    if (subject %in% c("pop", "all")) {
      syncMatrix <- overlap(scene, "overlap", compareToSelf = T)
      pairSync <- syncMatrix/scene$duration
    }

    syncMatrix2 <- overlap(scene, "overlap", compareToSelf)
    pairSync2 <- syncMatrix2/scene$duration

    indSync <- data.frame(id = scene$id, synchrony = -1)
    if (averageType == "mean") {
      indSync$synchrony <- rowMeans(pairSync2)
    } else if (averageType == "median") {
      indSync$synchrony <- row_medians(pairSync2)
    }

    popSync <- average(indSync[,2])

  } else if (method == "kempenaers") {
    indDaily <- receptivityByDay(scene)

    pairSync <- NULL

    indCols <- colSums(indDaily)
    indSync <- data.frame(id = scene$id, synchrony = -1)
    indSync$synchrony <- kemp_ind(indCols, scene$start, scene$end, scene$duration,
                                  compareToSelf)
    #     indSync$synchrony <- sapply(1:n, FUN = function(i) {
    #       inds <- as.character(scene[i, "start"]:scene[i, "end"])
    #       sum(indDaily[,inds]) - scene[i,"duration"]
    #     })/(scene[,"duration"]*(n-1))

    popSync <- average(indSync[,2])

  } else if (method == 'sync_prop') {
    days <- min(scene$start):max(scene$end)
    fl <- receptivityByDay(scene)
    n <- sum(fl)

    prop <- apply(fl, 2, function(x){sum(x)/n})
    indPropDaily<- t(apply(fl,1,function(x){x*prop}))
    totalIndProp <- rowSums(indPropDaily)

    pairSync <- NULL
    indSync <- data.frame(id = scene$id, synchrony = totalIndProp)
    popSync <- average(indSync[,2])

  } else if (method == "overlap") {
    if (subject %in% c("pop", "all")) {
      syncMatrix <- overlap(scene, "overlap", compareToSelf = T)
      eitherMatrix <- overlap(scene, "total", compareToSelf = T)
      pairSync <- syncMatrix/eitherMatrix
    }

    syncMatrix2 <- overlap(scene, "overlap", compareToSelf)
    eitherMatrix2 <- overlap(scene, "total", compareToSelf)
    pairSync2 <- syncMatrix2/eitherMatrix2

    indSync <- data.frame(id = scene$id, synchrony = -1)
    if (averageType == "mean") {
      indSync$synchrony <- rowMeans(pairSync2)
    } else if (averageType == "median") {
      indSync$synchrony <- row_medians(pairSync2)
    }

    popSync <- average(indSync[,2])

  } else if (method == "sync_nn") {
    if (syncNN >= n) {
      stop("syncNN must be less than n")
    }

    if (subject %in% c("pop", "all")) {
      syncMatrix <- overlap(scene, "overlap", compareToSelf = T)
      eitherMatrix <- overlap(scene, "total", compareToSelf = T)
      pairSyncInit <- syncMatrix/eitherMatrix
      pairSync <- matrix(nrow = n, ncol = n)
      for (i in 1:n) {
        pairSync[i,] <- sort(pairSyncInit[i,], decreasing = T)
      }
    }

    syncMatrix2 <- overlap(scene, "overlap", compareToSelf)
    eitherMatrix2 <- overlap(scene, "total", compareToSelf)
    pairSyncInit2 <- syncMatrix2/eitherMatrix2

    m <- ifelse(compareToSelf, n, n-1)
    pairSync2 <- matrix(nrow = n, ncol = m)
    for (i in 1:n) {
      pairSync2[i,] <- sort(pairSyncInit2[i,], decreasing = T)
    }

    indSync <- data.frame(id = scene$id, synchrony = -1)
    indSync$synchrony <- pairSync2[,syncNN]

    popSync <- average(indSync[,2])

  }


  # return
  potential <- list()
  if ("population" %in% subject) {
    potential$pop <- popSync
  }
  if ("individual" %in% subject) {
    potential$ind <- indSync
  }
  if ("pairwise" %in% subject) {
    potential$pair <- pairSync
  }
  if ("all" %in% subject) {
    potential$pop <- popSync
    potential$ind <- indSync
    potential$pair <- pairSync
  }
  attr(potential, "t") <- TRUE
  attr(potential, "s") <- FALSE
  attr(potential, "c") <- FALSE
  potential
}



