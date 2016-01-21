
#' @export
makeRLearner.regr.ranger = function() {
  makeRLearnerRegr(
    cl = "regr.ranger",
    package = "ranger",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "num.trees", lower = 1L, default = 500L),
      # FIXME: Add default value when data dependent defaults are implemented: mtry=floor(sqrt(#independent vars))
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerLearnerParam(id = "min.node.size", lower = 1L, default = 5L),
      makeLogicalLearnerParam(id = "replace", default = TRUE),
      makeNumericVectorLearnerParam(id = "split.select.weights", lower = 0, upper = 1),
      makeUntypedLearnerParam(id = "always.split.variables"),
      makeLogicalLearnerParam(id = "respect.unordered.factors", default = FALSE),
      makeDiscreteLearnerParam(id = "importance", values = c("none", "impurity", "permutation"), default = "none", tunable = FALSE),
      makeLogicalLearnerParam(id = "scale.permutation.importance", default = FALSE, requires = quote(importance == "permutation"), tunable = FALSE),
      makeIntegerLearnerParam(id = "num.threads", lower = 1L, when = "both", tunable = FALSE),
      makeLogicalLearnerParam(id = "save.memory", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = TRUE, when = "both", tunable = FALSE),
      makeIntegerLearnerParam(id = "seed", when = "both", tunable = FALSE),
      makeDiscreteLearnerParam(id = "se.method", default = "gower.knn", values = c("gower.knn"), when = "both"),
      makeIntegerLearnerParam(id = "gower.knn", default = 3L, lower = 1L, requires = quote(se.method == "gower.knn"), when = "predict")
    ),
    par.vals = list(num.threads = 1L, verbose = FALSE, se.method = "gower.knn", gower.knn = 3L),
    properties = c("numerics", "factors", "se"),
    name = "Random Forests",
    short.name = "ranger",
    note = "By default, internal parallelization is switched off (`num.threads = 1`) and `verbose` output is disabled. Both settings are changeable."
  )
}

#' @export
trainLearner.regr.ranger = function(.learner, .task, .subset, .weights, se.method, ...) {
  tn = getTaskTargetNames(.task)
  mod = ranger::ranger(formula = NULL, dependent.variable = tn, data = getTaskData(.task, .subset), write.forest = TRUE, ...)
  if (se.method == "gower.knn")
    return(attachTrainingInfo(mod, getTaskData(.task, .subset, target.extra = TRUE)$data))
  return(mod)
}

#' @export
predictLearner.regr.ranger = function(.learner, .model, .newdata, se.method, gower.knn, ...) {
  p = predict(object = .model$learner.model, data = .newdata, ...)
  if (.learner$predict.type == "se") {
    if (se.method == "gower.knn") {
      requirePackages("_StatMatch")
      x = getTrainingInfo(.model)
      gower.knn = min(gower.knn, nrow(x))
      dists = StatMatch::gower.dist(.newdata, x)
      se = apply(dists, 1L, function(x, nn) mean(head(sort(x, partial = nn), nn)), nn = gower.knn)
      return(cbind(p$predictions, se))
    }
  } else {
    return(p$predictions)
  }
}

if (FALSE) {
  task = bh.task
  n = getTaskSize(task)

  lrn = makeLearner("regr.ranger", predict.type = "se", gower.knn = 1)
  mod = train(learner = lrn, task = task)
  predict(mod, task) # se should be 0 because NN is 1

  lrn = makeLearner("regr.ranger", predict.type = "se", gower.knn = 3)
  mod = train(learner = lrn, task = task)
  predict(mod, task) # se should be > 0

  lrn = makeLearner("regr.ranger", predict.type = "se", gower.knn = 3)
  train.ind = sample(n, 2/3 * n)
  test.ind = setdiff(seq_len(n), train.ind)
  mod = train(learner = lrn, task = task, subset = train.ind)
  predict(mod, task) # se should be > 0
}
