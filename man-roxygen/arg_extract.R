#' @param extract [\code{function}]\cr
#'   Function used to extract information from a fitted model during resampling.
#'   Is applied to every \code{\link{WrappedModel}} resulting from calls to \code{\link{train}}
#'   during resampling.
#'   If no extraction should be performed set this value to \code{NULL}.
#'   The default is \code{"default"} and the following functions will be automatically applied for each wrapped learner used: 
#'   \itemize{
#'     \item \code{TuneWrapper}: \code{\link{getTuneResult}}
#'     \item \code{FeatSelWrapper}: \code{\link{getFeatSelResult}}
#'     \item \code{FilterWrapper}: \code{\link{getFilteredFeatures}}
#'   }
