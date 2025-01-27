#' Accessor Functions for \code{\link{nplr}} Objects
#'
#' Methods for extracting information from an object of class \code{nplr}. Each of the methods below is a convenience function that extracts the corresponding slot (as suggested by the method name) from the object of class \code{\link{nplr}}.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{signature(object = "nplr")}}{
#'   \itemize{
#'     \item \code{getX(object)}: Extracts the x-values.
#'     \item \code{getY(object)}: Extracts the y-values.
#'     \item \code{getXcurve(object)}: Extracts the x-values of the fitted curve.
#'     \item \code{getYcurve(object)}: Extracts the y-values of the fitted curve.
#'     \item \code{getFitValues(object)}: Extracts the fitted values.
#'     \item \code{getInflexion(object)}: Extracts the inflexion point.
#'     \item \code{getPar(object)}: Extracts the parameters of the logistic model.
#'     \item \code{getAUC(object)}: Extracts the area under the curve (AUC).
#'     \item \code{getGoodness(object)}: Extracts the goodness-of-fit.
#'     \item \code{getStdErr(object)}: Extracts the standard error of the model.
#'     \item \code{getWeights(object)}: Extracts the weights of the model.
#'   }
#'   }
#' }
#'
#' @seealso
#' \code{\link{nplr}}, \code{\link{getEstimates}}
#'
#' @name nplrAccessors
#' @docType methods
#' @aliases getX getX-methods getX,nplr-method
#'          getY getY-methods getY,nplr-method
#'          getXcurve getXcurve-methods getXcurve,nplr-method
#'          getYcurve getYcurve-methods getYcurve,nplr-method
#'          getFitValues getFitValues-methods getFitValues,nplr-method
#'          getInflexion getInflexion-methods getInflexion,nplr-method
#'          getPar getPar-methods getPar,nplr-method
#'          getAUC getAUC-methods getAUC,nplr-method
#'          getGoodness getGoodness-methods getGoodness,nplr-method
#'          getStdErr getStdErr-methods getStdErr,nplr-method
#'          getWeights getWeights-methods getWeights,nplr-method
#'          nplrAccessors nplrAccessors-methods nplrAccessors,nplr-method
#' @export

# -------------------------------------------------------
## SET GENERICS
# -------------------------------------------------------
setGeneric("getX", function(object) standardGeneric("getX"))
setGeneric("getY", function(object) standardGeneric("getY"))
setGeneric("getWeights", function(object) standardGeneric("getWeights"))
setGeneric("getFitValues", function(object) standardGeneric("getFitValues"))
setGeneric("getXcurve", function(object) standardGeneric("getXcurve"))
setGeneric("getYcurve", function(object) standardGeneric("getYcurve"))
setGeneric("getPar", function(object) standardGeneric("getPar"))
setGeneric("getInflexion", function(object) standardGeneric("getInflexion"))
setGeneric("getGoodness", function(object) standardGeneric("getGoodness"))
setGeneric("getStdErr", function(object) standardGeneric("getStdErr"))
setGeneric("getAUC", function(object) standardGeneric("getAUC"))
setGeneric("getEstimates",
	function(object, targets=seq(.9, .1, by=-.1), B=1e4, conf.level=.95)
		standardGeneric("getEstimates")
	)

# -------------------------------------------------------
## METHODS FOR EXTRACTING INFORMATION FROM THE nplr CLASS
# -------------------------------------------------------
setMethod("getX", "nplr", function(object) return(object@x))
setMethod("getY", "nplr", function(object) return(object@y))
setMethod("getWeights", "nplr", function(object) return(object@w))
setMethod("getFitValues", "nplr", function(object) return(object@yFit))
setMethod("getXcurve", "nplr", function(object) return(object@xCurve))
setMethod("getYcurve", "nplr", function(object) return(object@yCurve))
setMethod("getInflexion", "nplr", function(object) return(object@inflPoint))
setMethod("getPar", "nplr", function(object){
    return(list(npar=object@npars, params=object@pars))
    })
setMethod('getGoodness', 'nplr', function(object) return(object@goodness))
setMethod('getStdErr', 'nplr', function(object) return(object@stdErr))
setMethod("getAUC", "nplr", function(object) return(object@AUC))
