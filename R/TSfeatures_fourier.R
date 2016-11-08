#' @title Fast Fourier transform features
#'
#' @description The functino creates time series features based on the fast
#'   discrete fourier transform.
#'
#' @param curves \code{data.frame},\code{matrix}]\cr Time series curve data.
#' @param fft.coeff \code{character}\cr Optional. Which 'transformation' of the
#'   complex frequency domain representation should be calculated as feature
#'   representation. Must be one of \dQuote{amplitude} or \dQuote{Phase}.
#'   Default: \dQuote{amplitude}.
#' @return Returns an \code{data.frame} object containing the fourier
#'   coefficients.
#'
#' @export
getTSFourierFeatures = function(curves, fft.coeff = NULL) {
  requirePackages("stats", default.method = "load")

  if( is.null(fft.coeff) ) fft.coeff = "amplitude"

  if ( !(fft.coeff %in%  c("amplitude", "phase")) )
    stop("Transformation for complex frequency domain must be one of 'amplitude' or 'phase'. Please check method.")


  fftdata = t(apply(as.matrix(curves),1, fft))
  fftdata=as.data.frame(fftdata)

  switch(fft.coeff,
         amplitude = {ffttrafo = getFourierAmplitude(fourier.comp = fftdata)},
         phase = {ffttrafo = getFourierPhase(fourier.comp = fftdata)}
         )
  print(fft.coeff)
  
  if(!inherits(ffttrafo, "matrix"))
    ffttrafo = as.data.frame(matrix(ffttrafo, nrow = 1)) else
      ffttrafo = as.data.frame(ffttrafo)
  
  return(ffttrafo)
}

#' @export
getFourierAmplitude = function(fourier.comp){
  amp = apply(fourier.comp, 2, function(x) Re(x)^2 + Im(x)^2 ) # compute for each row
  amp = sqrt(amp)
  return(amp)
}

#' @export
getFourierPhase = function(fourier.comp){
  pha = apply(fourier.comp, 2, function(x) atan( Im(x) / Re(x) ) )
  return(pha)
}
