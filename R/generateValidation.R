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