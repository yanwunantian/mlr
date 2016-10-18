load_all()

set.seed(2)
configureMlr(show.info = TRUE, show.learner.output= TRUE)

gp = load2("gunpoint.RData")

task = makeTimeSeriesClassifTask(data = gp, target = "X1", positive = "1")

lrn = makeLearner("classif.rpart")

ptrain = function(data, target, args) {
  feats = setdiff(colnames(data), target)
  feats = sample(feats, 2)
  # print(feats)
  data = data[, c(feats, target)]
  control = list(feats = feats)
  list(data = data, control = control)
}

ppredict = function(data, target, args, control) {
  data = data[, control$feats]
  return(data)
}

lrn2 = makePreprocWrapper(lrn, train = ptrain, predict = ppredict)

# m = train(lrn2, task)
# p = predict(m, task)
r = crossval(lrn2, task)

