#' @title Internal construction / wrapping of time series features.
#'
#' @rdname TSfeatures
NULL

#' @export
#' @rdname TSfeatures
#' @export
TSfeatures = function() {
  UseMethod("TSfeatures")
}

#' @export
getTSfeaturesData <- function(task, method, pars) {
  z = getTaskData(task, target.extra = TRUE, recode.target = "-1+1")
  constructor = getS3method("TSfeatures", class = method)
  tsf = do.call(what = constructor, args = list(curves = z$data))
  tsf = cbind(as.factor(z$target), tsf)
  colnames(tsf)[1] <- task$task.desc$target
  return(tsf)
}
