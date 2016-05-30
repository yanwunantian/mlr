#' Generate the Validation data
#'
#' @param TuneResult 
#' @param include.diagnostics 
#'
#' @return what does it return
#' @export
generateValidationData = function(TuneResult, include.diagnostics = FALSE) {
  checkmate::assertClass(TuneResult, classes = c("TuneResult", "OptResult"))
  d = as.data.frame(TuneResult$opt.path)
  
  # convert params from factor to numeric where appropriate
  num_hypers = length(TuneResult$opt.path$par.set$pars)
  for (hyp in 1:num_hypers)
    d[, hyp] = type.convert(as.character(d[, hyp]))
  
  # most users probably won't use eol or error.message unless needed
  if (include.diagnostics == FALSE)
    d = within(d, rm(eol, error.message))
  
  # users might not know what dob means, so let's call it iters
  names(d)[names(d) == "dob"] = "iters"
  
  # create the S3 object
  makeS3Obj("ValidationData", data = d, measures = TuneResult$opt.path$y.names,
            hyperparams = names(TuneResult$x), 
            diagnostics = include.diagnostics, 
            optimization = getClass1(TuneResult$control))
}

#' Plot the Validation data
#'
#' @param ValidationData 
#' @param x.axis
#' @param y.axis
#' @param plot.type
#' @return what does it return
#' @export
plotValidationData = function(ValidationData, x.axis = NULL, y.axis = NULL, 
                              plot.type = "scatter"){
  # assertions
  checkmate::assertClass(ValidationData, classes = "ValidationData")
  if (is.null(x.axis))
    stopf("The x.axis argument must be specified!")
  if (is.null(y.axis))
    stopf("The y.axis argument must be specified!")
    
  data = ValidationData$data
  
  # assuming 1D for both x and y
  if (plot.type == "line")
    ggplot(data, aes_string(x = x.axis, y = y.axis)) + geom_line()
  if (plot.type == "scatter")
    ggplot(data, aes_string(x = x.axis, y = y.axis)) + geom_point()
  
}