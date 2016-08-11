#' @title Create a Data Source backend for mlr Tasks.
#'
#' @param src [\code{\link[base]{data.frame}} | \code{\link[dplyr]{tbl}}]\cr
#'   Source of the data. Currently supported
#'   \itemize{
#'     \item[\code{\link[base]{data.frame}}]{Regular data.frames.}
#'     \item[\code{\link[dplyr]{tbl}}]{Tables as used in \pkg{dplyr}. Can use a SQL backend.}
#'   }
#' @param rows [\code{integer} | \code{NULL}]\cr
#'   Subset of rows to use in the task. Additional rows are ignored.
#' @param cols [\code{character} | \code{NULL}]\cr
#'   Subset of columns to use in the task. Additional columns in \code{src} are ignored.
#' @param key [\code{character}]\cr
#'   Required for database backends: Name of the primary key column.
#' @param ... [\code{any}]\cr
#'   Additional arguments, depending on the type of \code{src}.
#' @return [\code{DataSource}].
#' @aliases DataSource
#' @export
makeDataSource = function(src, rows = NULL, cols = NULL, ...) {
  UseMethod("makeDataSource")
}

#' @export
makeDataSource.data.frame = function(src, rows = NULL, cols = NULL, ...) {
  assertCharacter(cols, any.missing = FALSE, null.ok = TRUE)
  ee = new.env(parent = emptyenv())
  ee$data = indexHelper(src, rows, cols)
  ee$cols = cols %??% names(src)
  class(ee) = c("DataSourceDataFrame", "DataSource")
  ee
}

#' @export
makeDataSource.tbl = function(src, rows = NULL, cols = NULL, key = NA_character_, ...) {
  requireNamespace("dplyr")

  assertCharacter(cols, any.missing = FALSE, null.ok = TRUE)
  assertString(key)
  ee = new.env(parent = emptyenv())
  ids = dplyr::collect(dplyr::select_(src, key))[[1L]]
  ee$data = src
  ee$cols = cols %??% setdiff(colnames(src), key)
  ee$ids = if (is.null(rows)) ids else intersect(ids, rows)
  ee$key = key
  class(ee) =  c("DataSourceTbl", "DataSource")
  ee
}

#' @title Retrive data from a DataSource.
#'
#' @param data [\code{\link{DataSource}}]\cr
#'    \code{\link{DataSource}}.
#' @param subset [\code{integer}]\cr
#'   Subset of rows to retrieve.
#' @param features [\code{character}]\cr
#'   Names of features to retrieve.
#' @return [\code{data.frame}].
#' @export
getData = function(src, subset, features) {
  UseMethod("getData")
}

#' @export
getData.DataSourceDataFrame = function(data, subset = NULL, features = NULL) {
  indexHelper(data$data, subset, features)
}

#' @export
getData.DataSourceTbl = function(data, subset = NULL, features = NULL) {
  requireNamespace("dplyr")
  requireNamespace("lazyeval")

  tmp = is.null(subset) && is.null(data$ids)
  if (is.null(subset) && is.null(data$ids)) {
      res = data$data
  } else {
    ids = if (!is.null(subset) && !is.null(data$ids)) intersect(subset, data$ids) else subset %??% data$ids
    res = dplyr::filter_(data$data, lazyeval::interp("key %in% ids", key = as.name(data$key), ids = ids))
  }

  if (is.null(features)) {
    res = dplyr::select_(res, ~dplyr::one_of(data$cols))
  } else {
    assertSubset(features, data$cols)
    res = dplyr::select_(res, ~dplyr::one_of(features))
  }
  dplyr::collect(res)
}

indexHelper = function(data, i, j) {
  switch(2L * is.null(i) + is.null(j) + 1L,
      data[i, j, drop = FALSE],
      data[i, , drop = FALSE],
      data[ , j, drop = FALSE],
      data)
}
