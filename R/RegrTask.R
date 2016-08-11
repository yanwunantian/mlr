#' @export
#' @rdname Task
makeRegrTask = function(id = deparse(substitute(data)), data, target, weights = NULL, blocking = NULL, check.data = TRUE) {
  assertString(id)
  assertDataFrame(data)
  assertString(target)
  assertFlag(check.data)

  task = makeSupervisedTask("regr", data, target, weights, blocking, check.data = check.data)

  if (check.data) {
    assertNumeric(data[[target]], any.missing = FALSE, finite = TRUE, .var.name = target)
  }

  task$task.desc = makeTaskDesc.RegrTask(task, id, target)
  addClasses(task, "RegrTask")
}

makeTaskDesc.RegrTask = function(task, id, target) {
  addClasses(makeTaskDescInternal(task, "regr", id, target), c("TaskDescRegr", "TaskDescSupervised"))
}
