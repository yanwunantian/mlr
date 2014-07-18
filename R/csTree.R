
csTreeInit = function(y, offset, parms, wt) {
  if (length(offset)) y = y - offset
  # FIXME: better summary fun, and other printers
  sfun = function(yval, dev, wt, ylevel, digits ) {
    paste(" mean=", format(signif(yval, digits)),
          ", MSE=" , format(signif(dev/wt, digits)),
          sep = '')
  }
  environment(sfun) = .GlobalEnv
  list(y = y, parms = NULL, numresp = ncol(y) + 1, numy = ncol(y), summary = sfun)
}

csTreeEval = function(y, wt, parms) {
  cm = colSums(y)
  j = which(cm == min(cm))
  if (length(j) > 1) {
    j = sample(j, 1)
  }
  list(label = c(j, cm), deviance = cm[j])
}

csTreeSplit = function(y, wt, x, parms, continuous) {
  n = nrow(y)
  goodness = numeric(n-1)
  if (continuous) {
    yy0 = colSums(y[1:n,,drop=FALSE] * wt[1:n])
    for (i in 1:(n-1)) {
      yy1 = colSums(y[1:i,,drop=FALSE] * wt[1:i])
      yy2 = colSums(y[(i+1):n,,drop=FALSE] * wt[(i+1):n])
      goodness[i] = min(yy0) - min(yy1) - min(yy2)
    }
    # FIXME direction
    list(goodness = goodness, direction = rep(-1, n-1))
  } else {
    # FIXME Categorical X variable
    stop("categ")
  }
}

csTreeSplit2 = function(y, wt, x, parms, continuous) {
  res = .Call("cstree_split", y, wt, x, parms, continuous)
  names(res) = c("goodness", "direction")
  res$direction = rep(-1, length(res$goodness))
  res
}

#' @title Fit a decision tree for a general cost-sensitive learning problem.
#'
#' @description
#' Fits an \code{\link[rpart]{rpart}} tree internally, but implements a new split criterion
#' to determine the tree. This criterion directly minimizes the natural loss function
#' for the algorithm selection problem, i.e. it computes the so called misclassification penalty,
#' which is defined to be the difference in performance between the predicted decision and
#' the oracle decision.
#'
#' @param feats [\code{data.frame}]\cr
#'   Features for modeling.
#' @param costs [\code{matrix} | \code{data.frame}]\cr
#'   A numeric matrix or data frame containing the costs of misclassification.
#'   We assume the general case of observation specific costs.
#'   This means we have n rows, corresponding to the observations, in the same order as \code{data}.
#'   The columns correspond to classes and their names are the class labels
#'   (if unnamed we use y1 to yk as labels).
#'   Each entry (i,j) of the matrix specifies the cost of predicting class j for observation i.
#' @param control [\code{list}]\cr
#'   Control object for \code{\link[rpart]{rpart}}.
#'   See \code{\link[rpart]{rpart.control}}.
#'   Default is setting \code{xval = 0}.
#' @param .preprocess [\code{logical(1)}]\cr
#'   Only for internal usage.
#'   Currently this ensures that all observations which are either
#'   unsolved by all algorithms or which have the same performance value for all
#'   algorithms are removed prior to training.
#' @return [\code{ASTree}].
#' @export
#' @aliases ASTree
csrpart = function(feats, costs, .preprocess = TRUE, .check = TRUE, ...) {
  if (.check) {
    assertClass(feats, "data.frame")
    assert(checkMatrix(costs, any.missing = FALSE), checkDataFrame(costs, any.missing = FALSE))
    if (is.data.frame(costs))
      costs = as.matrix(costs)
    assertNumeric(costs, lower = 0)
    if (is.null(colnames(costs)))
      colnames(costs) = paste("y", seq_col(costs), sep = "")
    checkColumnNames(costs)
    if (nrow(costs) != nrow(feats))
      stopf("Number of rows in cost matrix (%s) should equal the number of observations (%s).",
        nrow(costs), nrow(feats))
  }
  # if (.preprocess)
    # astask = removeObs(astask, unsolved = TRUE, all.same.perf = TRUE)
  cns = colnames(costs)
  costs = t(apply(costs, 1, function(x) x - min(x)))
  ulist = list(init = csTreeInit, eval = csTreeEval, split = csTreeSplit2)
  mydata  = cbind(feats, costs)
  form = as.formula(paste("cbind(", collapse(cns, ","), ")~."))
  mod = rpart(form, data = mydata, method = ulist, x = FALSE, y = FALSE, ...)
  mod$ylevels = cns
  addClasses(mod, "CSRPart")
}

#' Predict for \code{CSRPart} objects.
#'
#' @param object [\code{\link{CSRPart}}]\cr
#'   Model.
#' @param newdata [\code{data.frame}]\cr
#'   Features of new observations which should be predicted.
#' @param type [\code{character(1)}]\cr
#'   \dQuote{vector}: Predict factor of selected labels.\cr
#'   \dQuote{matrix}: Matrix of predicted costs, one column per label.\cr
#'   Default is \dQuote{vector}.
#' @param ... [any]\cr
#'   Currently ignored.
#' @return [\code{factor} | \code{matrix}].
#'   See above.
#' @export
predict.CSRPart = function(object, newdata, type = "vector", .check = TRUE, ...) {
  if (.check) {
    assertClass(newdata, "data.frame")
    assertChoice(type, c("vector", "matrix"))
  }
  p = rpart:::predict.rpart(object, newdata = newdata, type = type)
  if (type == "vector") {
    as.factor(object$ylevels[p])
  } else {
    p = p[,-1]
    setColNames(p, object$ylevels)
  }
}

