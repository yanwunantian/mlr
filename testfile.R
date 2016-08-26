setwd("/Users/Flo/work/mlr2/mlr")
load_all()
library("BBmisc")
# library("rpart")
source("/Users/Flo/work/mlr2/mlr/tests/testthat/helper_objects.R")
source("/Users/Flo/work/mlr2/mlr/tests/testthat/helper_helpers.R")
source("/Users/Flo/work/mlr2/mlr/tests/testthat/helper_learners_all.R")

task = subsetTask(binaryclass.task, subset = c(10:20, 180:190),
  features = getTaskFeatureNames(binaryclass.task)[12:15])
lrns = mylist(task, create = TRUE)
# lapply(lrns, testThatLearnerParamDefaultsAreTrueDefaults)


skip = c(19L, 24L, 30L, 49L, 57L, 58L)

for (i in seq_along(lrns)) {
  print(i)
  if (!(i %in% skip))
    testThatLearnerParamDefaultsAreTrueDefaults(lrns[[i]])
}


fixed = c(2L, 5L, 6L, 7L, 8L, 13L, 17L, 23L, 28L, 34L, 43L, 45L, 46L, 48L, 61L, 63L, 67L, 68L,
  69L)

length(skip) / length(lrns)

lrn = lrns[[49L]]
par.defaults = getParamSet(lrn)$pars
par.has.default = extractSubList(par.defaults, "has.default")
par.requires = extractSubList(par.defaults, "requires")
par.defaults = par.defaults[par.has.default]
par.defaults = extractSubList(par.defaults, "default", simplify = FALSE)
# drop params with mlr-specific default
par.defaults = par.defaults[which(names(par.defaults) %nin% names(getHyperPars(lrn)))]
set.seed(getOption("mlr.debug.seed"))
mod = train(lrn, binaryclass.task)
preds = predict(mod, binaryclass.task)$data$response
lrn = setHyperPars(lrn, par.vals = par.defaults)
lrn = setHyperPars(lrn, par.vals = list(rang = 0.7))
set.seed(getOption("mlr.debug.seed"))
mod.defaults = train(lrn, binaryclass.task)
# degree, scale, offset, order
preds.defaults = predict(mod.defaults, binaryclass.task)$data$response
all.equal(preds.defaults, preds)
