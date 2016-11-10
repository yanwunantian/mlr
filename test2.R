
load_all()

task = makeRegrTask(data = iris, target = "Petal.Width")
lrn = makeLearner("regr.rpart")
m = train(lrn, bh.task)
# z = computeAverageMarginalEffectsResampled(m, task = task, features = "Species")
z = computeAverageMarginalEffectsResampled(m, task = bh.task)
print(z)


ame = computeAverageMarginalEffects(m, bh.task)
rbindlist(ame)
ame[[2]]
