load_all()

print(getClassWeightParam("classif.randomForest"))

lrn1 = makeLearner("classif.randomForest")
lrn2 = makeWeightedClassesWrapper(lrn1, wcw.weight = 99)
m = train(lrn2, binaryclass.task)
p = predict(m, binaryclass.task)
cm = getConfMatrix(p)

# lrn1 = makeLearner("classif.randomForest")
# lrn2 = makeLearner("classif.svm")

# print(getClassWeightParam(lrn1))
# print(getClassWeightParam(lrn2))


# lrn1a = setClassWeights(lrn1, c(1, 3))
# print(lrn1a)
# lrn2a = setClassWeights(lrn2, c(1, 3))
# print(lrn2a)

# bls = list(lrn1, lrn2)
# lrn3 = makeModelMultiplexer(bls)
# # lrn4 = makeWeightedClassesWrapper(lrn3)


# print(getClassWeightParam(lrn3)$id)
# lrn3 = setHyperPars(lrn3, selected.learner = "classif.svm")
# print(getClassWeightParam(lrn3)$id)

