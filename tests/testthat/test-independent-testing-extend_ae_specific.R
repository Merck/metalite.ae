test <- prepare_ae_specific(meta_dummy(),
  population = "apat",
  observation = "wk12",
  parameter = "any"
)

# extend_ae_specific_inference:
# Throw error when ci is not a number and 0 >= ci >= 1.
# The CI matches that from SAS.

test_that("specific_inference throws error when !(0 >= ci >= 1)", {
  expect_error(extend_ae_specific_inference(test, ci = 1.1),
    regex = "Please choose a number 0 >= ci >= 1."
  )

  expect_error(extend_ae_specific_inference(test, ci = -.1),
    regex = "Please choose a number 0 >= ci >= 1."
  )

  expect_error(extend_ae_specific_inference(test, ci = "error"),
    regex = "ci must be a single numeric."
  )

  expect_error(extend_ae_specific_inference(test, ci = c(.1, .4)),
    regex = "ci must be a single numeric."
  )

  expect_silent(extend_ae_specific_inference(test, ci = .95))
})

test_that("specific_inference ci same as SAS", {
  ae_sp_inf_98 <- extend_ae_specific_inference(test, ci = .98)
})

# extend_ae_specific_duration:
# Throw errors when duration_var is not a string and of length == 1
# The average duration matches that from SAS

test_that("specific_duration: duration_var must be string", {
  expect_error(extend_ae_specific_duration(test,
    duration_var = c("ADURN", "ADUR")
  ),
  regexp = "duration_var must be a string"
  )

  expect_error(extend_ae_specific_duration(test,
    duration_var = c(1)
  ),
  regexp = "duration_var must be a string"
  )

  expect_error(extend_ae_specific_duration(test,
    duration_var = "ADUR"
  ),
  regexp = "does not exist in outdata"
  )

  expect_silent(extend_ae_specific_duration(test,
    duration_var = "ADURN"
  ))
})

# extend_ae_specific_events:
# The average number of events matches that from SAS

test_that("specific_events: average number of events same as SAS", {
  expect_silent(extend_ae_specific_events(test))

  ae_sp_ev <- extend_ae_specific_events(test)
})
