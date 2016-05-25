context("class.weights property and WeightedClassesWrapper")
# in test_learners_all_classif we the class.weights and the weighted class wrapper for all
# learners with the class weights property. so we can check only special stuff here.

test_that("class.weights property and WeightedClassesWrapper", {
  # check what happens, if learner does not support weights
  expect_error(makeWeightedClassesWrapper("classif.lda", wcw.weight = 1), "does not support")

  lrn1 = makeLearner("classif.randomForest")
  expect_is(getClassWeightParam(lrn1), "LearnerParam")
  lrn2 = setClassWeights(lrn1, c(1, 3))
  expect_equal(getHyperPars(lrn2), list(classwt = c(1, 3)))
})

test_that("WeightedClassesWrapper works with ModelMultiplexer", {
  lrn1 = makeLearner("classif.randomForest")
  lrn2 = makeLearner("classif.svm")
  bls = list(lrn1, lrn2)
  lrn3 = makeModelMultiplexer(bls)
  expect_true(hasLearnerProperties(lrn3, "class.weights"))
  # switch selected learner and see that class.weights param gets changed
  lrn3 = setHyperPars(lrn3, selected.learner = "classif.randomForest")
  expect_equal(getClassWeightParam(lrn3)$id, "classwt")
  lrn3 = setHyperPars(lrn3, selected.learner = "classif.svm")
  expect_equal(getClassWeightParam(lrn3)$id, "class.weights")

  lrn4 = makeWeightedClassesWrapper(lrn3)

})
