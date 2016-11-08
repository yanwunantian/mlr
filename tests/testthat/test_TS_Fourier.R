context("Ts_Fourier")

test_that("Ts_Fourier", {
  #load_all('~/mlr')
  #gp = data.frame(v1  =  t(1:4), X1= as.factor(1))
  gp = data.frame(v1 = 1:5, v2 = 2:6, v3 = 3:7, v4 = 4:8, X1= as.factor(c(-1,1,1,-1, 1)))
  
  taskTs = makeTimeSeriesClassifTask(data = gp, target = "X1", positive = "1")
  
  ## Specify the feature extraction method and generate task:
  ## We use the fourier features and the amplitude.
  taskFa = makeTSFeaturesClassifTask(task = taskTs, method = "fourier", pars = list(fft.coeff = "amplitude"))
  
  ## ... or we prefer the phase:
  taskFp = makeTSFeaturesClassifTask(task = taskTs, method = "fourier", pars = list(fft.coeff = "phase"))
  
  raw = as.matrix(gp[, !names(gp) %in% "X1"])
  
  pp = as.data.frame(t(apply(raw,1, fft)))
  
  B = getFourierAmplitude(pp)
  
  A = getTaskData(taskFa)
  A = A[, !names(A) %in% "X1"]
  
  
  expect_true(all(A ==B ))
  
  # test_fun = function(x) {
  #   return(4*x[,1]^2 - 2*x[,2])
  # }
  # n = 30
  # d = 2
  # set.seed(getOption("mlr.debug.seed"))
  # train.inds = 1:20
  # x = lhs::maximinLHS(n,d) 
  # y = test_fun(x)
  # GPfit.test.df = cbind.data.frame(x, y)
  # colnames(GPfit.test.df) = c("x1", "x2", "y")
  # m = GPfit::GP_fit(x[train.inds,], y[train.inds])
  # p = predict(m, xnew = x[-train.inds,])
  #testSimple("regr.GPfit", GPfit.test.df, "y", train.inds, p$Y_hat)
})
