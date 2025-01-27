#' Function to Estimate x Given y.
#'
#' This function takes as its first argument a model returned by \code{nplr()}. By inverting the logistic model, it estimates the x values corresponding to one (or a vector of) y target(s) provided. The standard error of the model, defined as the mean squared error on the fitted values, is used to estimate a confidence interval on the predicted x values, according to the specified \code{conf.level}. See \code{Details}.
#'
#' @param object an object of class \code{nplr}.
#' @param targets one, or a vector of, numerical value(s) for which the corresponding x has to be estimated. Default are target values from .9 to .1.
#' @param B the length of the y distribution from which the x confidence interval is estimated.
#' @param conf.level the estimated x confidence interval, bounded by (1-conf.level)/2 and 1 - (1-conf.level)/2 (by default .95, which gives x.025 and x.975).
#'
#' @details
#' In n-parameter logistic regressions, none of the parameters follow any particular distribution from which confidence intervals can be estimated. To overcome this issue, the standard error is used to generate a normal distribution of the target(s) passed to the function. The quantiles of that distribution are used in order to provide estimated bounds for the corresponding x value, with respect to \code{conf.level}.
#' See also \code{Warning}.
#'
#' @return A data set containing:
#' \item{y}{The target value.}
#' \item{x.05}{The lower bound of the estimated 95\% confidence interval (default). If another value is passed to conf.level, x will be labelled as x.(1-conf.level)/2.}
#' \item{x}{The estimated value.}
#' \item{x.95}{The upper bound of the estimated 95\% confidence interval (default). If another value is passed to conf.level, x will be labelled as x.1-(1-conf.level)/2.}
#'
#' @author
#' Frederic Commo, Brian M. Bot
#'
#' @section Warning:
#' Notice that, if any \eqn{target \leq B} or \eqn{target \geq T}, in other words outside the 2 asymptotes, the maximal (or minimal) possible value the model can estimate is returned.
#'
#' @seealso
#' \code{\link{nplr}}, \code{\link{plot.nplr}}, \code{\link{nplrAccessors}}
#'
#' @note
#' The data used in the examples are samples from the NCI-60 Growth Inhibition Data: \url{https://wiki.nci.nih.gov/display/NCIDTPdata/NCI-60+Growth+Inhibition+Data}, except for multicell.tsv which are simulated data.
#'
#' @examples
#' # Using the PC-3 data
#' require(nplr)
#' path <- system.file("extdata", "pc3.txt", package="nplr")
#' pc3 <- read.delim(path)
#' model <- nplr(x = pc3$CONC, y = pc3$GIPROP)
#' getEstimates(model)
#' getEstimates(model, c(.3, .6), conf.level = .9)
#'
#' @export
setMethod(
  f = "getEstimates",
  signature = "nplr",
  definition = function(object, targets, B, conf.level=.95){

    if(any(!is.numeric(targets)))
        stop("Target values have to be numeric.")

    pars <- getPar(object)
    if(any(targets<=pars$params$bottom)){
      targets[which(targets<=pars$params$bottom)] <- min(getFitValues(object))
      warning("One (or more) of the provided values were less or equal to the estimated bottom asymptote.",
              call.=FALSE, immediate.=TRUE)
      message("These values have been replaced by the minimal possible value the model can estimate.")
    }
    if(any(targets>=pars$params$top)){
      targets[which(targets>=pars$params$top)] <- max(getFitValues(object))
      warning("One (or more) of the values were greater or equal to the estimated top asymptote.",
              call.=FALSE, immediate.=TRUE)
      message("These values have been replaced by the maximal possible value the model can estimate.")
    }
    estim <- lapply(targets, function(target)
      .estimateRange(target, getStdErr(object), pars$params, B, object@useLog, conf.level)
    )
    estim <- cbind.data.frame(y=targets, do.call(rbind, estim))
    xlower <- sprintf("x.%s", substr((1-conf.level)/2, 3, 5))
    xupper <- sprintf("x.%s", substr(1-(1-conf.level)/2, 3, 5))
    colnames(estim)[-1] <- c(xlower, 'x', xupper)
    return(estim)
  }
)
