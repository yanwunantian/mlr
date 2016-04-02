
load_all()

task = makeRegrTask(data = iris, target = "Petal.Width")
lrn = makeLearner("regr.rpart")
m = train(lrn, task)
# z = computeAverageMarginalEffectsResampled(m, task = task, features = "Species")
z = computeAverageMarginalEffectsResampled(m, task = task)
print(z)
