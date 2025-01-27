#' Plotting \code{nplr} Objects
#'
#' This function allows visualizing logistic models fitted using \code{\link{nplr}}.
#'
#' @param x an object of class \code{\link{nplr}}.
#' @param pcol the points color.
#' @param lcol the line color.
#' @param showEstim logical/numeric. If a numerical value is passed (a y value to reach), the estimated x value, and interval, is displayed on the plot. Default is \code{FALSE}.
#' @param showCI logical. Show the estimated confidence interval.
#' @param showGOF logical. Show the estimated goodness-of-fit.
#' @param showInfl logical. Add the inflexion point on the plot.
#' @param showPoints logical. Add the points on the plot.
#' @param showSDerr logical. Add the standard errors on the plot (useful for experiments with replicates).
#' @param B the length of simulated y values. Used to estimate the confidence interval.
#' @param conf.level the confidence level. See \code{\link{getEstimates}}.
#' @param unit the unit to specify when \code{showEstim} is \code{TRUE}. Default is an empty string.
#' @param ... other graphical parameters. See \code{\link{par}}.
#'
#' @note
#' The data used in the examples are samples from the NCI-60 Growth Inhibition Data:
#' \url{https://wiki.nci.nih.gov/display/NCIDTPdata/NCI-60+Growth+Inhibition+Data}, except for multicell.tsv which are simulated data.
#'
#' @examples
#' require(nplr)
#' path <- system.file("extdata", "pc3.txt", package = "nplr")
#' pc3 <- read.delim(path)
#' model <- nplr(x = pc3$CONC, y = pc3$GIPROP)
#' plot(model, showEstim = 0.5, unit = "nM")
#'
#' @seealso
#' \code{\link{overlay}}
#'
#' @keywords datasets
#' @export

plot.nplr <- function(x, pcol="aquamarine1", lcol="red3",
                      showEstim=FALSE, showCI=TRUE, showGOF=TRUE, showInfl=FALSE,
                      showPoints = TRUE, showSDerr = FALSE,
                      B=1e4, conf.level=.95, unit="", ...){

    .plot(x, ...)
    if(showPoints) .addPoints(x, pcol, ...)
    if(showSDerr) .addErr(x, pcol, ...)
    if(showGOF) .addGOF(x)
    if(!(!showEstim)) .addEstim(x, showEstim, unit, B, conf.level)
    if(showCI) .addPolygon(x)
    if(showInfl) points(getInflexion(x), pch=19, cex=2, col="blue")
    .addCurve(x, lcol, ...)

    if(x@LPweight != 0){
        Sub = sprintf("Weighted %s-P logistic regr. (nplr package, version: %s)", x@npars, packageVersion("nplr"))
    } else{
        Sub = sprintf("Non-weighted %s-P logistic regr. (nplr package, version: %s)", x@npars, packageVersion("nplr"))
    }
    title(sub = Sub, cex.sub = .75)
}
