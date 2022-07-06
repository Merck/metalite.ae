test_that("'this'-> 'This', 'this is A Example' -> 'This is a example'", {
  expect_equal(metalite.ae:::to_sentence("this"), "This")
  expect_equal(metalite.ae:::to_sentence("this is A Example"), "This is a example")
})
