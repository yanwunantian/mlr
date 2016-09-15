#' @title Create a Data Source backend for mlr Tasks.
#'
#' @param src [\code{\link[base]{data.frame}} | \code{\link[dplyr]{tbl}}]\cr
#'   Source of the data. Currently supported
#'   \itemize{
#'     \item[\code{\link[base]{data.frame}}]{Regular data.frames.}
#'     \item[\code{\link[dplyr]{tbl}}]{Tables as used in \pkg{dplyr}. Can use a
#'     SQL backend.} .. }
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
  makeS3Obj(c("DataSourceDataFrame", "DataSource"),
    data = as.data.table(src),
    cols = cols %??% names(src),
    rows = rows %??% seq_row(src)
  )
}

#' @export
makeDataSource.tbl = function(src, rows = NULL, cols = NULL, key = NA_character_, ...) {
  assertCharacter(cols, any.missing = FALSE, null.ok = TRUE)
  assertString(key)

  makeS3Obj(c("DataSourceTbl", "DataSource"),
    data = src,
    cols = cols %??% setdiff(colnames(src), key),
    rows = .intersect(rows, dplyr::collect(dplyr::select_(src, key))[[1L]]),
    key = key
  )
}

#' @title Retrive data from a DataSource.
#'
#' @param data [\code{\link{DataSource}}]\cr
#'    \code{\link{DataSource}}.
#' @param rows [\code{integer}]\cr
#'   Subset of rows to retrieve.
#' @param cols [\code{character}]\cr
#'   Names of features to retrieve.
#' @return [\code{data.frame}].
#' @export
getData = function(data, rows = NULL, cols = NULL) {
  UseMethod("getData")
}

#' @export
getData.DataSourceDataFrame = function(data, rows = NULL, cols = NULL) {
  indexHelper = function(data, i, j) {
    switch(2L * is.null(i) + is.null(j) + 1L,
        data[i, j, drop = FALSE, with = FALSE],
        data[i, , drop = FALSE],
        data[ , j, drop = FALSE, with = FALSE],
        data)
  }
  res = setDF(indexHelper(data$data, .intersect(data$rows, rows), .intersect(data$cols, cols)))
  return(res)
}

#' @export
getData.DataSourceTbl = function(data, rows = NULL, cols = NULL) {
  rows = .intersect(data$rows, rows)
  cols = .intersect(data$cols, cols)
  res = if (is.null(rows)) data$data else dplyr::filter_(data$data, lazyeval::interp("key %in% ids", key = as.name(data$key), ids = rows))
  res = dplyr::select_(res, ~dplyr::one_of(cols))
  as.data.frame(dplyr::collect(res))
}

.intersect = function(x, y) {
    if (is.null(x)) return(y)
    if (is.null(y)) return(x)
    intersect(x, y)
}
