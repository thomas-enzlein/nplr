#' Plotting Multiple \code{nplr} Objects
#'
#' This function allows superimposing multiple logistic models fitted using \code{\link{nplr}}.
#'
#' @param modelList list. A list of objects of class \code{\link{nplr}}.
#' @param showLegend logical. Whether the legend should be displayed.
#' @param Cols character. A vector of colors to use. If \code{NULL} (default), greys will be used.
#' @param ... Other graphical parameters. See \code{\link{par}}.
#'
#' @details None
#'
#' @examples
#' path <- system.file("extdata", "multicell.tsv", package="nplr")
#' multicell <- read.delim(path)
#'
#' # Computing models (to store in a list)
#' cellsList <- split(multicell, multicell$cell)
#' Models <- lapply(cellsList, function(tmp) {
#'   nplr(tmp$conc, tmp$resp, silent = TRUE)
#' })
#'
#' # Visualizing
#' overlay(Models, xlab = expression(Log[10](Conc.)), ylab = "Resp.",
#'   main = "Superimposing multiple curves", cex.main = 1.5)
#'
#' @seealso \code{\link{plot.nplr}}
#' @name overlay
#' @export

overlay <- function(modelList = NULL, showLegend = TRUE, Cols = NULL, ...){

    if(is.null(modelList))
        stop("You just provided an empty list.")
    isValid <- sapply(modelList, function(tmp) inherits(tmp, "nplr"))

    if(!all(isValid))
        stop(sprintf("model #%s is not an instance of class 'nplr'", which(!isValid)))

    .multiCurve(modelList, showLegend, Cols, ...)
}
