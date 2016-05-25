context("learners_all_classif")

# test_that("learners work: classif ", {

#   # settings to make learners faster and deal with small data size
#   hyperpars = list(
#     classif.boosting = list(mfinal = 2L),
#     classif.cforest = list(mtry = 1L),
#     classif.bartMachine = list(verbose = FALSE, run_in_sample = FALSE,
#       # without this (and despite use_missing_data being TRUE), the test with missing data fails with a null point exception, which manifests itself as a completely different rJava error in the test
#       replace_missing_data_with_x_j_bar = TRUE,
#       num_iterations_after_burn_in = 10L),
#     classif.bdk = list(ydim = 2L),
#     classif.gbm = list(bag.fraction = 1, n.minobsinnode = 1),
#     classif.lssvm = list(kernel = "rbfdot", reduced = FALSE),
#     classif.nodeHarvest = list(nodes = 100L, nodesize = 5L),
#     classif.xyf = list(ydim = 2L)
#   )

#   fixHyperPars = function(lrn) {
#     if (lrn$id %in% names(hyperpars))
#       lrn = setHyperPars(lrn, par.vals = hyperpars[[lrn$id]])
#     return(lrn)
#   }

#   # binary classif
#   task = subsetTask(binaryclass.task, subset = c(10:20, 180:190),
#     features = getTaskFeatureNames(binaryclass.task)[12:15])
#   lrns = mylist(task, create = TRUE)
#   for (lrn in lrns) {
#     expect_output(print(lrn), lrn$id)
#     lrn = fixHyperPars(lrn)
#     m = train(lrn, task)
#     p = predict(m, task)
#     expect_true(!is.na(performance(p)))
#   }

#   # binary classif with factors
#   data = binaryclass.df[c(10:20, 180:190), 12:15]
#   data[, 4L] = factor(sample(c("a", "b"), size = nrow(data), replace = TRUE))
#   data$y = binaryclass.df[c(10:20, 180:190),binaryclass.target]
#   task = makeClassifTask(data = data, target = "y")
#   lrns = mylist(task, create = TRUE)
#   for (lrn in lrns) {
#     expect_output(print(lrn), lrn$id)
#     lrn = fixHyperPars(lrn)
#     m = train(lrn, task)
#     p = predict(m, task)
#     expect_true(!is.na(performance(p)))
#   }

#   # binary classif with prob
#   task = subsetTask(binaryclass.task, subset = c(1:10, 180:190),
#     features = getTaskFeatureNames(binaryclass.task)[12:15])
#   lrns = mylist(task, properties = "prob")
#   lrns = lapply(lrns$class, makeLearner, predict.type = "prob")
#   lapply(lrns, function(lrn) {
#     lrn = fixHyperPars(lrn)
#     m = train(lrn, task)
#     p = predict(m, task)
#     getPredictionProbabilities(p)
#     expect_true(!is.na(performance(p)))
#   })

#   # binary classif with weights
#   lrns = mylist("classif", properties = "weights", create = TRUE)
#   lapply(lrns, testThatLearnerRespectsWeights, hyperpars = hyperpars,
#     task = binaryclass.task, train.inds = binaryclass.train.inds, binaryclass.test.inds,
#     weights = rep(c(10000L, 1L), c(10L, length(binaryclass.train.inds) - 10L)),
#     pred.type = "prob", get.pred.fun = getPredictionProbabilities)

#   # classif with missing
#   d = binaryclass.df[c(1:10, 180:190), c(1:2, binaryclass.class.col)]
#   d[1, 1] = NA
#   task = makeClassifTask(data = d, target = binaryclass.target)
#   lrns = mylist(task, create = TRUE)
#   for (lrn in lrns) {
#     lrn = fixHyperPars(lrn)
#     m = train(lrn, task)
#     p = predict(m, task)
#     expect_true(!is.na(performance(p)))
#   }

#   # classif with factors
#   d = binaryclass.df[c(1:10, 181:190), c(1:2, binaryclass.class.col)]
#   d[, 2] = factor(rep(c("a", "b", "b", "a"), each = 5L))
#   task = makeClassifTask(data = d, target = binaryclass.target)
#   lrns = mylist(task, create = TRUE)
#   for (lrn in lrns) {
#     lrn = fixHyperPars(lrn)
#     m = train(lrn, task)
#     p = predict(m, task)
#     expect_true(!is.na(performance(p)))
#   }


# })


test_that("learners with class.weights params work", {

  testClassWeightsParam = function(lrn) {

    # test getter on learner string, then create, then test getter on learner object
    expect_is(getClassWeightParam(lrn), "LearnerParam")
    lrn = makeLearner(lrn)
    expect_is(getClassWeightParam(lrn), "LearnerParam")

    task = binaryclass.task
    rin = makeResampleInstance("Holdout", task)
    pos = getTaskDescription(task)$positive

    # return number of preds for class "cl" with weights w
    getClassPredCount = function(w, cl) {
      lrn2 = makeWeightedClassesWrapper(lrn, wcw.weight = w)
      r = resample(lrn2, task, rin)
      table(getPredictionResponse(r$pred))[[cl]]
    }

    # check that if we put more weight on pos class, we get more preds for pos class
    cm1 = getClassPredCount(0.0000001, pos)
    cm2 = getClassPredCount(1, pos)
    cm3 = getClassPredCount(1000000, pos)
    expect_true(cm1 <= cm2, info = lrn$id)
    expect_true(cm2 <= cm3, info = lrn$id)
    expect_true(cm1 < cm3, info = lrn$id)

    if (hasLearnerProperties(lrn, "multiclass")) {
      task = multiclass.task
      rin = makeResampleInstance("Holdout", task)
      classes = getTaskClassLevels(task)
      # check that if we put more weight on a specific class, we get more preds for that class
      for (cl in classes) {
        w1 = w2 = setNames(object = c(1, 1, 1), classes)
        w1[cl] = 1e-12
        w2[cl] = 1e+12
        cm1 = getClassPredCount(w1, cl)
        cm2 = getClassPredCount(w2, cl)
        expect_true(cm1 < cm2, info = lrn$id)
      }
    }
  }

  lrns = listLearners("classif", properties = "class.weights", create = FALSE)$class
  lapply(lrns, testClassWeightsParam)
})





