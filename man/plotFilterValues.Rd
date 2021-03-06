% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generateFilterValues.R
\name{plotFilterValues}
\alias{plotFilterValues}
\title{Plot filter values using ggplot2.}
\usage{
plotFilterValues(fvalues, sort = "dec", n.show = 20L,
  feat.type.cols = FALSE, facet.wrap.nrow = NULL, facet.wrap.ncol = NULL)
}
\arguments{
\item{fvalues}{[\code{\link{FilterValues}}]\cr
Filter values.}

\item{sort}{[\code{character(1)}]\cr
Sort features like this.
\dQuote{dec} = decreasing, \dQuote{inc} = increasing, \dQuote{none} = no sorting.
Default is decreasing.}

\item{n.show}{[\code{integer(1)}]\cr
Number of features (maximal) to show.
Default is 20.}

\item{feat.type.cols}{[\code{logical(1)}]\cr
Colors for factor and numeric features.
\code{FALSE} means no colors.
Default is \code{FALSE}.}

\item{facet.wrap.nrow, facet.wrap.ncol}{[\code{integer()}]\cr
Number of rows and columns for facetting. Default for both is \code{NULL}.
In this case ggplot's \code{facet_wrap} will choose the layout itself.}
}
\value{
ggplot2 plot object.
}
\description{
Plot filter values using ggplot2.
}
\examples{
fv = generateFilterValuesData(iris.task, method = "chi.squared")
plotFilterValues(fv)
}
\seealso{
Other filter: \code{\link{filterFeatures}},
  \code{\link{generateFilterValuesData}},
  \code{\link{getFilterValues}},
  \code{\link{getFilteredFeatures}},
  \code{\link{makeFilterWrapper}},
  \code{\link{plotFilterValuesGGVIS}}

Other generate_plot_data: \code{\link{generateCalibrationData}},
  \code{\link{generateCritDifferencesData}},
  \code{\link{generateFeatureImportanceData}},
  \code{\link{generateFilterValuesData}},
  \code{\link{generateFunctionalANOVAData}},
  \code{\link{generateLearningCurveData}},
  \code{\link{generatePartialDependenceData}},
  \code{\link{generateThreshVsPerfData}},
  \code{\link{getFilterValues}}
}

