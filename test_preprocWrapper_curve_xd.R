load_all()
library('wavelets')
set.seed(2)

configureMlr(show.info = TRUE, show.learner.output= TRUE)
gp = load2("gunpoint.RData")

task = makeTimeSeriesClassifTask(data = gp, target = "X1", positive = "1")

lrn = makeLearner("classif.rpart")

generateWaveletData = function(curves, target) {
  curves = curves[,setdiff(colnames(curves),target)]
  wtdata = NULL
  for (i in seq_row(curves)) {
    a = t(curves[i,])
    wt = dwt(a, filter = "haar", boundary = "periodic")
    wtdata = rbind(wtdata, unlist(c(wt@W,wt@V[[wt@level]])))
  }
  wtdata = as.data.frame(wtdata)
  return(wtdata)
}


ptrain = function(data, target, args) {
    control = list(fun=generateWaveletData)
    list(data = generateWaveletData(data), control = control)
 }

ppredict = function(data, target, args, control) {
   data = control$fun(data)
   return(data)
 }

lrn2 = makePreprocWrapper(lrn, train = ptrain, predict = ppredict)

model = train(lrn2, task, subset = 1:50)
pred = predict(model, task, subset = 51:200)
p = performance(pred, measures = list(mmce, tpr))
print(p)
