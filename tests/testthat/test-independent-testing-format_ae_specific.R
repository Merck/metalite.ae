test <- prepare_ae_specific(
  meta_example(),
  population = "apat",
  observation = "wk12",
  parameter = "any"
)

test_that("When Mock is True masked value should have value of xx, x", {
  test1 <- format_ae_specific(
    test,
    display = c("n", "prop", "total"),
    mock = TRUE
  )
  expect_true(all(c("xx", " x", NA, "(xx.x)", " (x.x)", " xx", "  x", "xxx") %in% unique(unlist(test1$tbl[-1]))))
})

test_that("When display contains n and prop then the total column disappear", {
  dis <- c("n", "prop")
  test1 <- format_ae_specific(
    test,
    display = dis,
    mock = FALSE
  )

  ncnt <- ncol(test1$n)
  total <- "total" %in% dis

  if ("n" %in% dis) {
    if (total) {
      n <- test1$n
    } else {
      n <- test1$n[, -ncnt]
    }
    cntn <- names(n)
  }

  if ("prop" %in% dis) {
    if (total) {
      pop <- test1$prop
    } else {
      pop <- test1$prop[, -ncnt]
    }
    cntpop <- names(pop)
  }

  nprop <- sort(c(cntn, cntpop))

  var <- sort(names(test1$tbl[-1]))

  expect_equal(nprop, var)
})


test_that("When display contains n, prop and diff then one has additional column ", {
  dis <- c("n", "prop", "diff")
  test1 <- format_ae_specific(
    test,
    display = dis,
    mock = FALSE
  )

  ncnt <- ncol(test1$n)
  total <- "total" %in% dis

  if ("n" %in% dis) {
    if (total) {
      n <- test1$n
    } else {
      n <- test1$n[, -ncnt]
    }
    cntn <- names(n)
  }

  if ("prop" %in% dis) {
    if (total) {
      pop <- test1$prop
    } else {
      pop <- test1$prop[, -ncnt]
    }
    cntpop <- names(pop)
  }

  if ("diff" %in% dis) {
    dif <- names(test1$diff)
  }

  npropdiff <- sort(c(cntn, cntpop, dif))

  vardif <- sort(names(test1$tbl[-1]))

  expect_equal(npropdiff, vardif)
})

test_that("When display contains n, prop and diff_ci then one has an additional confidence interval column ", {
  disp <- c("n", "prop", "diff_ci")

  tbl <- test |>
    extend_ae_specific_inference() |>
    format_ae_specific(display = disp)

  expect_equal(
    c(
      "name",
      "n_1", "prop_1",
      "n_2", "prop_2",
      "n_3", "prop_3",
      "ci_2", "ci_3"
    ),
    names(tbl$tbl)
  )
})

test_that("Between and Within are properly ordered when total is specified", {
  disp <- c("n", "prop", "dur", "events_avg", "diff", "diff_ci", "diff_p", "total")

  tbl <- test |>
    extend_ae_specific_duration(duration_var = "ADURN") |>
    extend_ae_specific_events() |>
    extend_ae_specific_inference() |>
    format_ae_specific(display = disp)

  expect_equal(
    c(
      "name",
      "n_1", "prop_1", "dur_1", "eventsavg_1",
      "n_2", "prop_2", "dur_2", "eventsavg_2",
      "n_3", "prop_3", "dur_3", "eventsavg_3",
      "n_4", "prop_4", "dur_4", "eventsavg_4",
      "diff_2", "ci_2", "p_2",
      "diff_3", "ci_3", "p_3"
    ),
    names(tbl$tbl)
  )
})

test_that("Between and Within are properly ordered when total is not specified", {
  disp <- c("n", "prop", "dur", "events_count", "diff", "diff_ci", "diff_p")

  tbl <- test |>
    extend_ae_specific_duration(duration_var = "ADURN") |>
    extend_ae_specific_events() |>
    extend_ae_specific_inference() |>
    format_ae_specific(display = disp)

  expect_equal(
    c(
      "name",
      "n_1", "prop_1", "dur_1", "eventscount_1",
      "n_2", "prop_2", "dur_2", "eventscount_2",
      "n_3", "prop_3", "dur_3", "eventscount_3",
      "diff_2", "ci_2", "p_2",
      "diff_3", "ci_3", "p_3"
    ),
    names(tbl$tbl)
  )
})


test_that("When display contains n, prop and dur then one has an additional duration column ", {
  disp <- c("n", "prop", "dur")

  tbl <- test |>
    extend_ae_specific_duration(duration_var = "ADURN") |>
    format_ae_specific(display = disp)

  expect_equal(
    c(
      "name",
      "n_1", "prop_1", "dur_1",
      "n_2", "prop_2", "dur_2",
      "n_3", "prop_3", "dur_3"
    ),
    names(tbl$tbl)
  )
})


test_that("When display contains n,prop and dur then one has an additional column of average duration of AE ", {
  disp <- c("n", "prop", "dur")

  tbl <- test |>
    extend_ae_specific_duration(duration_var = "ADURN") |>
    format_ae_specific(display = disp)

  expect_equal(
    c(
      "name",
      "n_1", "prop_1", "dur_1",
      "n_2", "prop_2", "dur_2",
      "n_3", "prop_3", "dur_3"
    ),
    names(tbl$tbl)
  )
})


test_that("When display contains n, prop and events then one has an additional column on the average number of AE ", {
  disp <- c("n", "prop", "events_avg")
  tbl <- test |>
    extend_ae_specific_events() |>
    format_ae_specific(display = disp)

  expect_equal(
    c(
      "name",
      "n_1", "prop_1", "eventsavg_1",
      "n_2", "prop_2", "eventsavg_2",
      "n_3", "prop_3", "eventsavg_3"
    ),
    names(tbl$tbl)
  )
})
