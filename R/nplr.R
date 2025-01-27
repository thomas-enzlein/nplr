#' Function to Fit n-Parameter Logistic Regressions.
#'
#' This function computes a weighted n-parameters logistic regression, given x (typically compound concentrations) and y values (responses: optic densities, fluorescence, cell counts,...). See Details.
#'
#' @param x a vector of numeric values, e.g. a vector of drug concentrations.
#' @param y a vector of numeric values, e.g. a vector of responses, typically provided as proportions of control.
#' @param useLog Logical. Should x-values be Log10-transformed. Default to \code{TRUE}, set to \code{FALSE} if x is already in Log10.
#' @param LPweight a coefficient to adjust the weights. \eqn{LPweight = 0} will compute a non-weighted np-logistic regression.
#' @param npars a numeric value (or \code{"all"}) to specify the number of parameters to use in the model. If \code{"all"} the logistic model will be tested with 2 to 5 parameters, and the best option will be returned. See Details.
#' @param method a character string to specify what weight method to use. Options are \code{"res" (Default), "sdw", "gw"}. See Details.
#' @param silent Logical. Specify whether \code{warnings} and/or \code{messages} have to be silenced. Default to \code{FALSE}.
#'
#' @details
#' The 5-parameter logistic regression is of the form:
#'
#' \deqn{ y = B + (T - B)/[1 + 10^(b*(xmid - x))]^s }
#'
#' where \code{B} and \code{T} are the bottom and top asymptotes, respectively, \code{b} and \code{xmid} are the Hill slope and the x-coordinate at the inflexion point, respectively, and \code{s} is an asymmetric coefficient. This equation is sometimes referred to as the Richards' equation.
#'
#' When specifying \code{npars = 4}, the \code{s} parameter is forced to be \code{1}, and the corresponding model is a 4-parameter logistic regression, symmetrical around its inflexion point. When specifying \code{npars = 3} or \code{npars = 2}, additional constraints force \code{B} and \code{T} to be \code{0} and \code{1}, respectively.
#'
#' Weight methods:
#'
#' The model parameters are optimized using \code{\link[stats]{nlm}}, given a sum of squared errors function, \eqn{sse(Y)}, to minimize:
#'
#' \deqn{sse(Y) = \Sigma [W.(Yobs - Yfit)^2 ]}
#'
#' where \eqn{Yobs}, \eqn{Yfit}, and \eqn{W} are the vectors of observed values, fitted values, and weights, respectively.
#'
#' - Residual weights (\code{"res"}): \deqn{W = (1/residuals)^LPweight}, where \code{residuals} are the squared error between observed and fitted values, and \code{LPweight} is a tuning parameter.
#' - Standard weights (\code{"sdw"}): \deqn{W = 1/Var(Yobs_r)}, where \code{Var(Yobs_r)} is the vector of within-replicates variances.
#' - General weights (\code{"gw"}): \deqn{W = 1/Yfit^LPweight}, where \code{Yfit} are the fitted values.
#'
#' The standard and general weight methods are described in the references.
#'
#' @return An object of class \code{nplr}.
#'
#' @section Slots:
#' \itemize{
#'   \item \code{x}: the x values used in the model, which can be \code{Log10(x)} if \code{useLog = TRUE}.
#'   \item \code{y}: the y values.
#'   \item \code{useLog}: logical.
#'   \item \code{npars}: the best number of parameters if \code{npars = "all"}, or the specified number otherwise.
#'   \item \code{LPweight}: the weights tuning parameter.
#'   \item \code{yFit}: the y fitted values.
#'   \item \code{xCurve}: the x values generated to draw the curve (200 points between \code{min} and \code{max} of x).
#'   \item \code{yCurve}: the fitted values corresponding to \code{xCurve}.
#'   \item \code{inflPoint}: the inflexion point x and y coordinates.
#'   \item \code{goodness}: the goodness-of-fit (correlation between fitted and observed y values).
#'   \item \code{stdErr}: the mean squared error between fitted and observed y values.
#'   \item \code{pars}: the model parameters.
#'   \item \code{AUC}: the area under the curve, estimated using both the trapezoid method and Simpson's rule.
#' }
#'
#' @references
#' 1. Richards, F. J. (1959). A flexible growth function for empirical use. J Exp Bot 10, 290-300.
#' 2. Giraldo J, Vivas NM, Vila E, Badia A. Assessing the (a)symmetry of concentration-effect curves: empirical versus mechanistic models. Pharmacol Ther. 2002 Jul;95(1):21-45.
#' 3. Motulsky HJ, Brown RE. Detecting outliers when fitting data with nonlinear regression - a new method based on robust nonlinear regression and the false discovery rate. BMC Bioinformatics. 2006 Mar 9;7:123.
#'
#' @author
#' Frederic Commo, Brian M. Bot
#'
#' @seealso
#' \code{\link{convertToProp}}, \code{\link{getEstimates}}, \code{\link{plot.nplr}}, \code{\link{nplrAccessors}}
#'
#' @note
#' The data used in the examples are samples from the NCI-60 Growth Inhibition Data: \url{https://wiki.nci.nih.gov/display/NCIDTPdata/NCI-60+Growth+Inhibition+Data}, except for multicell.tsv which are simulated data.
#'
#' @examples
#' require(nplr)
#' path <- system.file("extdata", "pc3.txt", package = "nplr")
#' pc3 <- read.delim(path)
#' model <- nplr(x = pc3$CONC, y = pc3$GIPROP)
#' plot(model)
#' @export
nplr <- function(x, y, useLog=TRUE, LPweight=0.25,
                npars="all", method=c("res", "sdw", "gw"),
                silent=FALSE){

    if(length(x)!=length(y))
        stop("x and y lengths differ.")

    if(is.numeric(npars) & (npars<2 | npars>5))
        stop("\n'npars' must be in [2, 5], or 'all'!")

    method <- match.arg(method)

    repTable <- table(x)
    maxrep <- max(repTable, na.rm=TRUE)
    minrep <- min(repTable, na.rm=TRUE)
    if(method=="sdw"){
        if(maxrep<2){
            method <- "res"
            if(!silent){
                warning("\nNone of the x-values seem to be replicated.
                    The 'sdw' method has been replaced by 'res'.",
                    call.=FALSE, immediate.=TRUE)
                message()
            }

        } else if(minrep<2){
            if(!silent){
                warning("\nOne (or more) points have no replicates.
                    The 'sdw' method may not be appropriate.",
                    call.=FALSE, immediate.=TRUE)
                message()
            }
            }
    }

    if(method=="gw" & any(y<0))
        if(!silent){
            warning("\nBecause of one (or more) y negative values,
                the 'gw' method may not be appropriate.",
                call.=FALSE, immediate.=TRUE)
            message()
        }
    if(any(is.na(x) | is.na(y))){
        NAs <- union(which(is.na(x)), which(is.na(y)))
        x <- x[-NAs]
        y <- y[-NAs]
        if(!silent){
            warning(call.=FALSE,
                sprintf("%s point(s) has(ve) been removed for missingness.",
                    length(NAs)),
                immediate.=TRUE)
            message()
                }
    }
    y <- y[order(x)]
    x <- sort(x)

    pp <- sum(y<0 | y>1)/length(y)
    if(pp > .2 & !silent){
        warningtext <- "% of your y values fall outside the range [0, 1]"
        warning(call.=FALSE,
            sprintf("%s%s", round(pp*100, 2), warningtext),
            immediate.=TRUE)
        message("\t- any results output may not be representative.")
        message("\t- be sure you are using y-values as proportions.")
        message()
    }

    if(useLog) x <- log10(x)
    object <- new("nplr", x=x, y=y, useLog=useLog, LPweight=LPweight)
    object@call <- match.call()
    object@weightMethod <- method

    .weight <- .chooseWeight(method)

    if(npars=="all"){
        testAll <- .testAll(.sce, x, y, .weight, LPweight, silent)
        npars <- testAll$npars
        if(!silent){
            msg <- sprintf("The %s-parameters model showed better performance",
                format(npars))
            message(msg)
        }
    }

    nPL <- .chooseModel(npars)
    inits <- .initPars(x, y, npars)
    options(warn = -1)
    best <- nlm(f=.sce, p=inits, x=x, yobs=y, .weight, LPweight, nPL)
    options(warn = 0)
    if(best$iterations==0)
        stop("'nlm' failed to estimate parameters.\n")

    # Best estimates
    bottom <- best$estimate[1]
    top <- best$estimate[2]
    xmid<-best$estimate[3]
    scal <- best$estimate[4]
    s <- best$estimate[5]

    # Estimating values
    newX <- seq(min(x), max(x), length=200)
    newY <- nPL(bottom, top, xmid, scal, s, newX)
    yFit <- nPL(bottom, top, xmid, scal, s, x)
    if(length(unique(signif(yFit, 5)))==1)
        stop("nplr failed and returned constant fitted values.
            Your data may not be appropriate for such model.")

    # Inflexion point coordinates
    pars <- cbind.data.frame(bottom=bottom, top=top, xmid=xmid, scal=scal, s=s)
    infl <- .inflPoint(pars)

    # Performances
    w <- .weight(x, y, yFit, LPweight)
    w <- w/sum(w, na.rm = TRUE)
    perf <- .getPerf(y, yFit, w)

    object@w <- w
    object@npars <- npars
    object@pars <- pars
    object@yFit <- yFit
    object@xCurve <- newX
    object@yCurve <- newY
    object@inflPoint <- infl
#    object@goodness <- perf$goodness
    object@goodness <- .gof(y, yFit, w)
    object@stdErr <- c(stdErr = perf$stdErr, "weighted stdErr" = perf$wStdErr)
    object@AUC <- data.frame(trapezoid = .AUC(newX, newY), Simpson = .Simpson(newX, newY))

    return(object)
}
