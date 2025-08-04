test_that("empty_table produces consistent rtf output", {
  # Call the function with example parameters
  rtf_output <- empty_table(
    title = "Test Title",
    orientation = "portrait",
    text_font_size = 12
  )

  # Use snapshot testing to capture the output
  expect_snapshot_output(print(rtf_output))
})