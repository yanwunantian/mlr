context("DataSource")

test_that("DataSource constructors", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("lazyeval")
  skip_if_not_installed("RSQLite")
  requireNamespace("dplyr")
  requireNamespace("RSQLite")


  # Local backend
  srcs = list(local = iris)

  # SQLite backend
  db = dplyr::src_sqlite(tempfile(fileext = ".sql"), create = T)
  df = iris
  df$.row = seq_len(nrow(df))
  dplyr::copy_to(db, df, name = "data", temporary = FALSE, indexes = list(".row"))
  srcs$sqlite = dplyr::tbl(db, "data")


  for (src in srcs) {
    data = makeDataSource(src, key = ".row")
    expect_is(data, "DataSource")
    expect_data_frame(getData(data, features = "Species"), nrow = 150, ncol = 1)
    expect_data_frame(getData(data, subset = 21:30, features = "Species"), nrow = 10, ncol = 1)
    expect_equal(data.table::uniqueN(getData(data, subset = 49:51, features = "Species")$Species), 2L)
    expect_equal(as.data.frame(getData(data))[, 1:4], iris[, 1:4])

    data = makeDataSource(src, cols = "Sepal.Length", key = ".row")
    expect_data_frame(getData(data), ncol = 1, nrow = 150)

    data = makeDataSource(src, rows = 101:110, cols = c("Species", "Sepal.Length"), key = ".row")
    expect_data_frame(getData(data), ncol = 2, nrow = 10)
  }
})
