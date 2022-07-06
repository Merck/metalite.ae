
test_that("If path_outdata = ... is not null, then the Rdata will be saved", {
  path <- paste0(tempdir(), "/iris.Rdata")
  expect_message(rtf_output(iris,
             path_outdata = path,
             path_outtable = NULL))
  expect_true(file.exists(path))

})

test_that("If path_outtable = ... is not null, then the rtf will be saved", {
  path <- paste0(tempdir(), "/iris.rtf")
  expect_message(rtf_output(iris,
             path_outdata = NULL,
             path_outtable = path))
  expect_true(file.exists(path))
})