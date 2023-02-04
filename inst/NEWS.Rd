\name{NEWS}
\title{News for Package \pkg{mateable}}


\section{Changes in version 0.3.2 (2022-01-13)}{
  \itemize{
    \item \code{makeScene} now includes an argument, \code{split}, to split a dataframe by the level of a specified variable.
    \item \code{plotScene} now includes arguments \code{sortBy} and \code{colorBy} to allow users to control the stacking order and color of temporal plots by specified variables.
    \item In all plotting functions, users can now set their own plot margins (\code{oma} and \code{mar}) outside of the function, using \code{par}.
    \item We removed the argument to add a main title, main, to grant users more flexibility in formatting their plot titles.
    \item \code{synchrony} has a new \code{method} option, called \code{sync_prop}, which calculates individual synchrony the proportion of a seasons reproductive opportunities that occur on days that an individual reproduced.
    \item Population values of \code{sync_prop} calculate the mean individual synchrony.
    \item A new function \code{simplify.potential.list} simplifies the list output of the \code{synchrony}, \code{proximity}, and \code{compatibility} functions to a dataframe.
    \item We updated the vignette investigateMatingScene to reflect new features and included a new dataset, \code{ech2012}, which includes spatial and temporal data for two populations.
    \item The plot limits shown by \code{plotScene} for multi-year scenes are now accurate in all years.
    \item \code{proximity} no longer throws an error in calculating measures proximity for multi-year scenes.
  }
}
