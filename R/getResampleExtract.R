#In the following we have the functions
# - getReampleExtract2: Returns a feasable function we can use as extract in resample() for each specific learner class. 
#   All is handled recursively because we have wrapped learners. 
#   getLearnerResampleExtractFunction2 returns a list of all appliable functions
#
# - getLearnerResampleExtractFunction converts the list of functions obtained by getLearnerResampleExtractFunction2 to a single function which returns a list of each function result.

getLearnerResampleExtractFunction2 = function(learner) {
  UseMethod("getLearnerResampleExtractFunction2")
}

getLearnerResampleExtractFunction2.default = function(learner) {
  stopf("Wrapper without underlying Learner.")
}

getLearnerResampleExtractFunction = function(learner){
  functions = getLearnerResampleExtractFunction2(learner)
  if (length(functions) == 0L)
    function(x) NULL
  else
    lapply(functions, function(fun) fun(x))
}
