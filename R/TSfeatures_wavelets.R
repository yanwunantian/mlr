#' @title Wavelet features
#'
#'@description Extracts time series wavelet features.
#'
#' @export
TSfeatures.Wavelets = function(curves) {
  requirePackages("wavelets", default.method = "load")

  wtdata = NULL
  for (i in seq_row(curves)) {
    a = t(curves[i,])
    wt = dwt(a, filter = "haar", boundary = "periodic")
    wtdata = rbind(wtdata, unlist(c(wt@W,wt@V[[wt@level]])))
  }
  wtdata = as.data.frame(wtdata)
  return(wtdata)
}
