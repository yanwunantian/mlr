#' @title Fuse learner with the uncertainty estimation
#'
#' @template arg_learner
#' @template ret_learner
#' @family wrapper
#' @export
makeUncertaintyWrapper = function(learner, gower.knn = 3) {
  learner = checkLearner(learner, type="regr")
  gower.knn = asInt(gower.knn, lower = 1L)
  ps = makeParamSet(
    makeIntegerLearnerParam(id = "gower.knn", lower = 1L, when = "predict")
  )
  pv = list(gower.knn = gower.knn)
  makeBaseWrapper(
    id = paste(learner$id, "uncertainty", sep = "."),
    type = "regr",
    next.learner = learner,
    par.set = ps,
    par.vals = pv,
    learner.subclass = "UncertaintyWrapper", 
    model.subclass = "UncertaintyModel"
    )
}

#' @export
trainLearner.UncertaintyWrapper = function(.learner, .task, .subset, .weights = NULL, ...) {
  .task = subsetTask(.task, .subset)
  model = train(.learner$next.learner, .task, weights = .weights)
  m = makeChainModel(next.model = model, cl = "UncertaintyModel")
  m$origignal.task = .task
  return(m)
}

#' @export
predictLearner.UncertaintyWrapper = function(.learner, .model, .newdata, gower.knn, ...) {
  p = predict(getLearnerModel(.model), newdata = .newdata, ...)
  if (.learner$predict.type == "se") {
    requirePackages("_StatMatch")
    train.data = getTaskData(.model$learner.model$origignal.task, target.extra = TRUE)$data
    gower.knn = min(gower.knn, nrow(train.data))
    dists = StatMatch::gower.dist(.newdata, train.data)
    se = apply(dists, 1L, function(x, nn) mean(head(sort(x, partial = nn), nn)), nn = gower.knn)
    return(cbind(getPredictionResponse(p), se)) 
  } else {
    return(getPredictionResponse(p))
  }
}

# we need to override here. while the predtype of the encapsulated learner must always
# be response, we can estimates probs and se on the outside
#' @export
setPredictType.UncertaintyWrapper = function(learner, predict.type) {
  setPredictType.Learner(learner, predict.type)
}

#' @export
getLearnerProperties.UncertaintyWrapper = function(learner) {
  union(getLearnerProperties(learner$next.learner), "se")
}
