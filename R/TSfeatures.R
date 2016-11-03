#' @title Create task on time series features.
#'
#' @description The function creates a feature representation of raw time series
#'   for a time series classification task. The method used to create this
#'   feature representation must be specified by the user in \code{method}.
#'   Since the resulting data does not contain temporal structure anymore, the
#'   returned task is a \code{ClassifTask}.
#'
#'
#' @param task [\code{TimeSeriesClassifTask}]\cr Time series classification
#'   task.
#' @param method [\code{character(1)}]\cr Which method is used to create time
#'   series features. Two methods available. Wavelet transformation:
#'   \dQuote{wavelets} Fourier transformation: \dQuote{fourier}
#' @param pars  \cr Further parameters passed as argument e.g., for feature
#'   representation methods.
#' @return [\code{ClassifTask}].
#' @export
makeTSFeaturesClassifTask = function(task, method, pars = NULL) {

  #check for Time Series Classif Task
  if ( !any(class(task) == "TimeSeriesClassifTask") )
    stop("Task is not a 'TimeSeriesClassifTask'. Please check task.")
  #check valid feature method
  if ( !(method %in%  c("wavelets", "fourier")) )
    stop("Method for feature extraction must be one of 'wavelets' or 'fourier'. Please check method.")

  z = getTaskData(task, target.extra = TRUE)
  switch(method,
         wavelets = {tsf = getTSWaveletFeatures(curves = z$data, pars = pars)},
         fourier = {tsf = getTSFourierFeatures(curves = z$data)}
         )
  tsf = cbind(as.factor(z$target), tsf)
  # rename target column
  colnames(tsf)[1] <- task$task.desc$target
  newtask = makeClassifTask(data = tsf, target = task$task.desc$target, positive = task$task.desc$positive)
  return(newtask)

}
