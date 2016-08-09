#' @export
makeDataSource = function(x, cols = NULL, ...) {
  UseMethod("makeDataSource")
}

#' @export
makeDataSource.data.frame = function(x, cols = NULL, ...) {
  assertCharacter(cols, any.missing = FALSE, null.ok = TRUE)
  ee = new.env(parent = emptyenv())
  ee$data = x
  ee$cols = cols %??% names(x)
  class(ee) = c("DataLocal", "Data")
  ee
}

#' @export
makeDataSource.tbl = function(x, cols = NULL, key = NULL, ...) {
  assertCharacter(cols, any.missing = FALSE, null.ok = TRUE)
  assertString(key)
  ee = new.env(parent = emptyenv())
  ee$data = x
  ee$cols = cols %??% setdiff(colnames(x), key)
  ee$ids = collect(select_(x, key))[[1L]]
  ee$key = key
  class(ee) =  c("DataRemote", "Data")
  ee
}

getData = function(data, subset, features) {
  UseMethod("getData")
}

getData.DataLocal = function(data, subset = NULL, features = NULL) {
  switch(2L * is.null(subset) + is.null(features) + 1L,
      data$data[subset, features, drop = FALSE],
      data$data[subset, , drop = FALSE],
      data$data[ , features, drop = FALSE],
      data$data)
}

getData.DataRemote = function(data, subset = NULL, features = NULL) {
  if (is.null(subset)) {
    res = data$data
  } else {
    res = filter_(data$data, interp("key %in% ids", key = as.name(data$key), ids = subset))
  }

  if (is.null(features)) {
    res = select_(res, ~one_of(data$cols))
  } else {
    assertSubset(features, data$cols)
    res = select_(res, ~one_of(features))
  }
  collect(res)
}


if (FALSE) {
  library(dplyr)
  library(lazyeval)
  library(checkmate)

  db = src_sqlite(tempfile(fileext = ".sql"), create = T)
  df = iris
  df$.row = seq_len(nrow(df))
  src = copy_to(db, df, name = "data", temporary = FALSE, indexes = list(".row"))
  x = tbl(db, "data")

  data = makeDataSource(x, key = ".row")
  getData(data, 20:60, features = "Species")

  data = makeDataSource(iris)
  getData(data, 20:60)
}
