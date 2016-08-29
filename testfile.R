setwd("/Users/Flo/work/mlr2/mlr")
load_all()
library("BBmisc")
# library("rpart")
source("/Users/Flo/work/mlr2/mlr/tests/testthat/helper_objects.R")
source("/Users/Flo/work/mlr2/mlr/tests/testthat/helper_helpers.R")
source("/Users/Flo/work/mlr2/mlr/tests/testthat/helper_learners_all.R")

task = subsetTask(binaryclass.task, subset = c(1:50, 151:200),
  features = getTaskFeatureNames(binaryclass.task)[12:15])
lrns = mylist(task, create = TRUE)
# lapply(lrns, testThatLearnerParamDefaultsAreTrueDefaults)


skip = c(24L, 49L, 58L)

for (i in seq_along(lrns)) {
  print(i)
  if (!(i %in% skip))
    testThatLearnerParamDefaultsAreTrueDefaults(lrns[[i]], task)
}

# regr learners

task = subsetTask(regr.num.task, subset = c(1:50, 151:200),
  features = getTaskFeatureNames(regr.num.task)[1:4])
lrns = mylist(task, create = TRUE)
# lapply(lrns, testThatLearnerParamDefaultsAreTrueDefaults)


skip = c(34L)

for (i in seq_along(lrns)) {
  print(i)
  if (!(i %in% skip))
    testThatLearnerParamDefaultsAreTrueDefaults(lrns[[i]], task)
}


# surv learners

task = surv.task
lrns = mylist(surv.task, create = TRUE)
skip = c(4L)

for (i in seq_along(lrns)) {
  print(i)
  if (!(i %in% skip))
    testThatLearnerParamDefaultsAreTrueDefaults(lrns[[i]], task)
}

# cluster learners

task = noclass.task
lrns = mylist(task, create = TRUE)
skip = c(1L, 9L)

for (i in seq_along(lrns)) {
  print(i)
  if (!(i %in% skip))
    testThatLearnerParamDefaultsAreTrueDefaults(lrns[[i]], task)
}




# debug
# task = surv.task
lrn = lrns[[58L]]
par.defaults = getParamSet(lrn)$pars
par.has.default = extractSubList(par.defaults, "has.default")
par.requires = extractSubList(par.defaults, "requires")
par.defaults = par.defaults[par.has.default]
par.defaults = extractSubList(par.defaults, "default", simplify = FALSE)
# drop params with mlr-specific default
par.defaults = par.defaults[which(names(par.defaults) %nin% names(getHyperPars(lrn)))]
set.seed(getOption("mlr.debug.seed"))
mod = train(lrn, task)
preds = predict(mod, task)$data$response
lrn = setHyperPars(lrn, par.vals = par.defaults)
# lrn = setHyperPars(lrn, par.vals = list(rang = 0.7))
set.seed(getOption("mlr.debug.seed"))
mod.defaults = train(lrn, task)
# degree, scale, offset, order
preds.defaults = predict(mod.defaults, task)$data$response
all.equal(preds.defaults, preds)
