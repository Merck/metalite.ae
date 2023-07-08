library(dplyr)
library(metalite.ae)
library(metalite)

adsl <- r2rtf::r2rtf_adsl
adsl$TRTA <- adsl$TRT01A
adsl$TRTA <- factor(adsl$TRTA,
  levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
)

adae <- r2rtf::r2rtf_adae
adae$TRTA <- factor(adae$TRTA,
  levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
)

adae$subline <- paste0("Age=", adae$AGE, ", ID=", adae$USUBJID, ", Gender=", adae$SEX)

# Removing labels

remo_label <- function(a) {
  for (name in colnames(a)) {
    attr(a[[name]], "label") <- NULL
  }
  return(a)
}

# Creating example listing

meta_ae_listing_example <- function() {
  plan <- plan(
    analysis = "ae_listing", population = "apat",
    observation = "wk12", parameter = c("any", "rel", "ser")
  )

  meta_adam(
    population = adsl,
    observation = adae
  ) |>
    define_plan(plan = plan) |>
    define_population(
      name = "apat",
      group = "TRTA",
      subset = quote(SAFFL == "Y")
    ) |>
    define_observation(
      name = "wk12",
      group = "TRTA",
      subset = quote(SAFFL == "Y"),
      label = "Weeks 0 to 12",
      rel_day = "ASTDY"
    ) |>
    define_parameter(
      name = "rel",
      subset = quote(AEREL %in% c("POSSIBLE", "PROBABLE"))
    ) |>
    define_parameter(
      name = "ser",
      subset = quote(AESER == "Y")
    ) |>
    define_analysis(
      name = "ae_listing",
      var_name = c("USUBJID", "ASTDY", "AEDECOD", "ADURN", "AESEV", "AESER", "AEREL", "AEOUT"),
      subline = "subline",
      subline_by = NULL,
      group_by = c("USUBJID", "ASTDY"),
      page_by = c("TRTA", "subline")
    ) |>
    meta_build()
}

listing_ae <- full_join(
  adsl |> select(USUBJID, TRT01AN, ITTFL), # Merge with adsl
  adae,
  by = "USUBJID",
  multiple = "all"
)

test_that("Its class is 'outdata'", {
  output <- prepare_ae_listing(meta_ae_listing_example(), "ae_listing", "apat", "wk12", "ser")
  expect_equal(class(output), "outdata")
  expect_equal(output$population, "apat")
  expect_equal(output$parameter, "ser")
  expect_equal(output$n, NULL)
  expect_equal(output$order, NULL)
  expect_equal(output$group, NULL)
  expect_equal(output$reference_group, NULL)
  expect_equal(names(output$col_name), c("USUBJID", "ASTDY", "AEDECOD", "ADURN", "AESEV", "AESER", "AEREL", "AEOUT", "subline", "TRTA"))
})


test_that("Checking Serious AE records at WK12", {
  d <- prepare_ae_listing(meta_ae_listing_example(), "ae_listing", "apat", "wk12", "ser")
  prod_tbl <- d$tbl
  rownames(prod_tbl) <- NULL

  listi <- listing_ae |>
    filter(SAFFL == "Y" & AESER == "Y") |>
    select(c("USUBJID", "ASTDY", "AEDECOD", "ADURN", "AESEV", "AESER", "AEREL", "AEOUT", "subline", "TRTA"))

  rownames(listi) <- NULL
  attr(listi$TRTA, "label") <- "TRTA"
  attr(listi$subline, "label") <- "subline"

  # expect_equal(prod_tbl,listi)

  expect_equal(get_label(prod_tbl), get_label(listi))

  prod_tbl <- remo_label(prod_tbl)
  listi <- remo_label(listi)

  expect_equal(prod_tbl, listi)
})

test_that("Checking AE related records at WK12", {
  d <- prepare_ae_listing(meta_ae_listing_example(), "ae_listing", "apat", "wk12", "rel")
  prod_tbl <- d$tbl
  rownames(prod_tbl) <- NULL


  listi <- listing_ae |>
    filter(SAFFL == "Y" & AEREL %in% c("POSSIBLE", "PROBABLE")) |>
    select(c("USUBJID", "ASTDY", "AEDECOD", "ADURN", "AESEV", "AESER", "AEREL", "AEOUT", "subline", "TRTA"))

  rownames(listi) <- NULL
  attr(listi$TRTA, "label") <- "TRTA"
  attr(listi$subline, "label") <- "subline"


  expect_equal(get_label(prod_tbl), get_label(listi))

  prod_tbl <- remo_label(prod_tbl)
  listi <- remo_label(listi)

  expect_equal(prod_tbl, listi)
})
