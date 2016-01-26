context("UncertaintyWrapper")

test_that("UncertaintyWrapper", {
  # regression
  lrn1 = makeLearner("regr.rpart")
  lrn2 = makeUncertaintyWrapper(lrn1, gower.knn = 3L)
  set.seed(1)
  m = train(lrn2, regr.task)
  p = predict(m, regr.task)
  lrn2 = setPredictType(lrn2, "se")
  set.seed(1)
  m = train(lrn2, regr.task)
  p2 = predict(m, regr.task)
  pse = getPredictionSE(p2)
  expect_true(!any(is.na(pse)))
  expect_equal(getPredictionResponse(p), getPredictionResponse(p2))
})

