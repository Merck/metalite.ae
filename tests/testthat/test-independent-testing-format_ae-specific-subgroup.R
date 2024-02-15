meta <- meta_ae_example()
outdata <- prepare_ae_specific_subgroup(meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel",
  subgroup_var = "SEX",
  display_subgroup_total = FALSE
)

test_that("When display is n, prop only display n and prop", {
  test1 <- format_ae_specific_subgroup(
    outdata,
    display = c("n", "prop"),
    mock = FALSE
  )
  expect_equal(test1$display, c("n", "prop"))
  expect_true(all(c("Fn_1", "Fprop_1", "Fn_2", "Fprop_2", "Fn_3", "Fprop_3", "Mn_1", "Mprop_1", "Mn_2", "Mprop_2", "Mn_3", "Mprop_3") %in% unique(unlist(names(test1$tbl)))))
})

test_that("When display is n, prop, Total only display n, prop and Total", {
  test2 <- format_ae_specific_subgroup(
    outdata,
    display = c("n", "prop", "Total"),
    mock = FALSE
  )
  expect_equal(test2$display, c("n", "prop", "Total"))
  expect_true(all(c("Fn_1", "Fprop_1", "Fn_2", "Fprop_2", "Fn_3", "Fprop_3", "Fn_4", "Fprop_4", "Mn_1", "Mprop_1", "Mn_2", "Mprop_2", "Mn_3", "Mprop_3", "Mn_4", "Mprop_4") %in% unique(unlist(names(test2$tbl)))))
})

test_that("When digits_prop = 2", {
  test3 <- format_ae_specific_subgroup(
    outdata,
    display = c("n", "prop"),
    digits_prop = 2,
    mock = TRUE
  )
  expect_equal(unlist(unique(test3$tbl$Fprop_1)), c(NA, "(xx.xx)", " (x.xx)"))
})

outdata1 <- prepare_ae_specific_subgroup(meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel",
  subgroup_var = "SEX",
  display_subgroup_total = TRUE
)

test_that("When display is n, prop only display n and prop", {
  test4 <- format_ae_specific_subgroup(
    outdata1,
    display = c("n", "prop"),
    mock = FALSE
  )
  expect_equal(test4$display, c("n", "prop"))

  x <- gsub("[n_1n_2n_3]", "", names(test4$tbl))
  x <- gsub("prop", "", x)
  expect_true(all(c("F", "M", "Total") %in% unique(unlist(x))))
})
