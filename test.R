library(mlr)
library(ICEbox)
library(randomForest)
library(MASS) #has Boston Housing data, Pima


# ICEbox example
set.seed(20160402)

########  regression example
data(Boston) #Boston Housing data
X = Boston
y = X$medv
X$medv = NULL

## build a RF:
bhd_rf_mod = randomForest(X, y)


## usually crashes!!!
## Create an 'ice' object for the predictor "age":
bhd.ice = ice(object = bhd_rf_mod, X = X, y = y, predictor = "age", frac_to_build = .1)

plot(bhd.ice)


# mlr example
set.seed(20160402)

task = makeRegrTask(id = "bhd", data = Boston, target = "medv")
lrn = makeLearner("regr.randomForest")
mod = train(lrn, task)
pd.ind.regr = generatePartialDependenceData(mod, task, c("age"), individual = TRUE)
plotPartialDependence(pd.ind.regr)

