library(dplyr)
library(tidyr)
library(metalite)

meta <- meta_ae_example()
outdata_year <- prepare_ae_summary(meta,
  population = "apat",
  observation = "wk12",
  parameter = "any"
) %>%
  extend_ae_summary_eaer(adj_unit = "year")

outdata_month <- prepare_ae_summary(meta,
  population = "apat",
  observation = "wk12",
  parameter = "any"
) %>%
  extend_ae_summary_eaer(adj_unit = "month")

adsl <- r2rtf::r2rtf_adsl |> dplyr::mutate(TRTA = TRT01A, TRTAN = TRT01AN)
adae <- r2rtf::r2rtf_adae


# output total in adsl
adsl_total <- adsl %>%
  group_by(USUBJID) %>%
  summarize(across(everything(), first)) %>%
  mutate(
    TRTA = "Total",
    TRTAN = 99
  )

adsl_new <- bind_rows(adsl, adsl_total) %>% arrange(USUBJID)

# output total in adae
adae_total <- adae %>%
  mutate(TRTA = as.character(TRTA), TRTAN = as.integer(TRTAN)) %>%
  bind_rows(., mutate(., TRTA = "Total", TRTAN = 99))

adae_new <- adae_total

# Create table a
a <- adsl_new %>%
  dplyr::group_by(TRTA, TRTAN) %>%
  dplyr::summarize(tot = sum(TRTDUR), n = dplyr::n(), .groups = "drop") %>%
  dplyr::arrange(TRTAN)

# Create table b
b <- adae_new %>%
  dplyr::group_by(TRTA, TRTAN) %>%
  dplyr::summarize(cnt = dplyr::n(), .groups = "drop") %>%
  dplyr::arrange(TRTAN)

# Merge table a and b
ab <- merge(a, b, by = c("TRTA", "TRTAN")) %>%
  dplyr::arrange(TRTAN)

# Calculate eaer value by month and year in table ab
ab1 <- ab %>%
  mutate(
    trty = tot / 365.24,
    trtm = tot / 30.4367,
    eaer_all_year = (cnt * 100) / trty,
    eaer_all_month = (cnt * 100) / trtm
  )

ab2y <- data.frame(
  TRTA = c("Placebo", "Low Dose", "High Dose", "Total"),
  Value = ab1$eaer_all_year
)




ab3y <- ab2y %>%
  pivot_wider(names_from = TRTA, values_from = Value)


ab4y <- as.data.frame(ab3y)
eaer_yr <- as.data.frame(outdata_year$eaer)


# TEST 1
# Define a test using test_that
test_that("Comparing specific values from datasets", {
  # Check if the values are equal
  expect_equal(ab4y, eaer_yr)
})


#  TEST 2

ab2m <- data.frame(
  TRTA = c("Placebo", "Low Dose", "High Dose", "Total"),
  Value = ab1$eaer_all_month
)




ab3m <- ab2m %>%
  pivot_wider(names_from = TRTA, values_from = Value)


ab4m <- as.data.frame(ab3m)
eaer_mo <- as.data.frame(outdata_month$eaer)



# Define a test using test_that
test_that("Comparing specific values from datasets", {
  # Check if the values are equal
  expect_equal(ab4m, eaer_mo)
})
