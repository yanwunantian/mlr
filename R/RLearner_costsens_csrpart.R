#' @export
makeRLearner.costsens.csrpart = function() {
  makeRLearnerCostSens(
    cl = "costsens.csrpart",
    package = c("rpart", "mlr"),
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "minsplit", default = 20L, lower = 1L),
      makeIntegerLearnerParam(id = "minbucket", lower = 1L),
      makeNumericLearnerParam(id = "cp", default = 0.01, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "maxcompete", default = 4L, lower = 0L),
      makeIntegerLearnerParam(id = "maxsurrogate", default = 5L, lower = 0L),
      makeDiscreteLearnerParam(id = "usesurrogate", default = 2L, values = 0:2),
      makeDiscreteLearnerParam(id = "surrogatestyle", default = 0L, values = 0:1),
      # we use 30 as upper limit, see docs of rpart.control
      makeIntegerLearnerParam(id = "maxdepth", default = 30L, lower = 1L, upper = 30L),
      makeIntegerLearnerParam(id = "xval", default = 0L, lower = 0L),
      makeUntypedLearnerParam(id = "parms")
    ),
    properties = c("twoclass", "multiclass", "numerics")
  )
}

#' @export
trainLearner.costsens.csrpart = function(.learner, .task, .subset, .weights = NULL, ...) {
  feats = getTaskData(.task, .subset)
  cost = getTaskCosts(.task, .subset)
  csrpart(feats, cost, ...)
}

#' @export
predictLearner.costsens.csrpart = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, ...)
}
