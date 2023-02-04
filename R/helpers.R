# a function to convert a mating summary list to a dataframe
matingSummary.df <- function(matSum){
  if(is.list(matSum[[1]][[1]])){
    df <- lapply(matSum, matingSummary.df)
  }
  if(is.list(matSum[[1]][1])){
    l <- lapply(matSum, unlist)
    df <- as.data.frame(do.call(rbind, l))
    if('popSt' %in% colnames(df)){
      df$popSt <- as.Date(df$popSt, origin = '1970-01-01')
      df$peak <- as.Date(df$peak, origin = '1970-01-01')
      df$meanSD <- as.Date(df$meanSD, origin = '1970-01-01')
      df$meanED <- as.Date(df$meanED, origin = '1970-01-01')
      df$popEnd <- as.Date(df$popEnd, origin = '1970-01-01')
    }
    df$id <- rownames(df)
    if (!all(df$id == as.character(df$year))){
      df <- df[,c(ncol(df),1:(ncol(df)-1))]
    } else{
      df <- df[,1:(ncol(df)-1)]
    }
    rownames(df) <- NULL
  } else {
    df <- as.data.frame(matSum)
  }
  df
}


simplify.potential.list <- function(s, subject){
  subject <- match.arg(subject, c("population", "pairwise",
                                  "individual", "all"),
                       several.ok = TRUE)
  if('all' %in% subject){
    subject <- c('population','pairwise','individual')
  }
  potential <- list()
  if ('population' %in% subject){
    pop <- data.frame(pop = names(s), synchrony = sapply(s,function(l)l[[which('pop' == names(l))]]))
    row.names(pop) <- NULL
    potential$pop <- pop
  }
  if ('individual' %in% subject){
    ind <- as.data.frame(do.call(rbind,lapply(s,function(l)l[[which('ind' == names(l))]])))
    row.names(ind) <- NULL
    potential$ind <- ind
  }
  # if ('pairwise' %in% subject){
  #   pair <- array(unlist(lapply(s,function(l)l[[which('pair' == names(l))]])), dim  = c(dim(s[[1]][[which('pair' == names(s[[1]]))]])[1],dim(s[[1]][[which('pair' == names(s[[1]]))]])[1],3))
  #   potential$pair <- pair
  # }
  return(potential)
}
