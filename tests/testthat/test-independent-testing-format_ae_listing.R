x <- meta_ae_test()

outdata <- prepare_ae_listing(
  x,
  analysis = "ae_listing",
  population = "apat",
  observation = "wk12",
  parameter = "rel"
)

test_that("format_ae_listing returns outdata with organized listing columns", {
  tbl <- format_ae_listing(outdata)

  expect_equal(class(tbl), "outdata")
  expect_true(is.data.frame(tbl$tbl))
  expect_equal(names(tbl$tbl), names(tbl$col_name))
})

test_that("format_ae_listing supports mock output", {
  tbl <- format_ae_listing(outdata, mock = TRUE)

  expect_true(nrow(tbl$tbl) <= 20)
})
