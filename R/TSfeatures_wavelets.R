#' @title Discrete Wavelet transform features
#'
#' @description The function extracts discrete wavelet transform coefficients
#'   for the raw time series curve data.
#'
#' @param curves [\code{data.frame},\code{matrix}]\cr Time series curve data.
#' @param filter, periodic \cr Optional. Default: \code{filter} = \dQuote{la8},
#'   \code{boundary} = \dQuote{periodic}. See package \code{wavelets} for more
#'   information.
#' @return Returns an \code{data.frame} object containing the wavelet
#'   coefficients.
#'
#' @export
getTSWaveletFeatures = function(curves, pars = NULL) {
  requirePackages("wavelets", default.method = "load")

  wfilter = pars$filter
  wboundary = pars$boundary
  if (is.null(pars$filter)) wfilter = "la8"
  if (is.null(pars$boundary)) wboundary = "periodic"

  wtdata = NULL
  for (i in seq_row(curves)) {
    a = t(curves[i,])
    wt = wavelets::dwt(a, filter = wfilter, boundary = wboundary)
    wtdata = rbind(wtdata, unlist(c(wt@W,wt@V[[wt@level]])))
  }
  wtdata = as.data.frame(wtdata)
  return(wtdata)
}
