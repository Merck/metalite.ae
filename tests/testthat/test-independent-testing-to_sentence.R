test_that("'this' -> 'This', 'this is An Example' -> 'This is an example'", {
  expect_equal(metalite.ae:::to_sentence("this"), "This")
  expect_equal(metalite.ae:::to_sentence("this is An Example"), "This is an example")
})
