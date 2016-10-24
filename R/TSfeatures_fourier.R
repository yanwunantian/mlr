# To fix: complex numbers
#' @title Fourier features
#'
#' @description Extracts time series features based on fourier analysis.
#'
#' @export
TSfeatures.Fourier = function(curves) {
  requirePackages("stats", default.method = "load")

  fftdata = fft(as.matrix(curves))
  fftdata = as.data.frame(fftdata)
  return(fftdata)
}
