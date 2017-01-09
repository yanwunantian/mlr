#' @title Sets class weights for classification learners that support the property.
#'
#' @template arg_learner_classif
#' @return ret_learner
#' @export
setClassWeights = function(learner, w) {
  checkLearnerClassif(learner)
  p = getClassWeightParam(learner)
  setHyperPars(learner, par.vals = setNames(list(w), p$id))
}
