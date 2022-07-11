# test_that("multiplication works", {
# est plan:

#  The output is a list
# Test each component
# population: match the expectation
# parameter: match the expectation
# n: match that from SAS (take r2rtf::r2rtf_adsl and r2rtf::r2rtf_adae as an example)
# order: match the expectation
# group: match the expectation
# reference_group: match the expectation
# prop: match that from SAS (take r2rtf::r2rtf_adsl and r2rtf::r2rtf_adae as an example)
# diff: match that from SAS (take r2rtf::r2rtf_adsl and r2rtf::r2rtf_adae as an example)
# name: match the expectation
# })


# devtools::load_all()
library(metalite.ae)
library(metalite)
library(haven)
library(dplyr)
library(tidyr)
library(testthat)

adsl <- r2rtf::r2rtf_adsl
adae <- r2rtf::r2rtf_adae %>% mutate(TRT01A = TRTA)

adsl %>%
  group_by(TRT01P) %>%
  summarise(n = n())


sum_par <- "any;rel;nonser;ser;ser0rel;dth;dtc0rel;disc;disc0drel;disc0ser;disc0ser0rel"
plan <- plan(
  analysis = "ae_summary",
  population = "apat",
  observation = "trtemfl",
  parameter = sum_par,
  pilot = TRUE
)

adsl$TRT01P <- factor(adsl$TRT01P, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
adsl$TRT01A <- factor(adsl$TRT01A, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
# adae$TRT01P <- factor(adae$TRT01P, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
adae$TRT01A <- factor(adae$TRT01A, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))

meta <- meta_adam(
  observation = adae,
  population = adsl
)

plan_pilot <- plan %>%
  subset(pilot) %>%
  mutate(display_total = analysis == "ae_summary")

meta <- meta %>%
  define_plan(plan_pilot)

meta <- meta %>%
  define_population(
    name = "apat",
    group = "TRT01A",
    subset = ITTFL == "Y",
    label = "(All Participants as Treated)"
  )

meta <- meta %>% define_observation(
  name = "trtemfl",
  group = "TRT01A",
  var = "AEDECOD",
  subset = TRTEMFL == "Y",
  label = "Treatment emergent"
)

meta <- meta %>%
  define_parameter(name = "any") %>%
  define_parameter(
    name = "rel",
    subset = AEREL == "RELATED",
    label = "drug-related adverse events"
  ) %>%
  define_parameter(
    name = "nonser",
    subset = AESER != "Y" | AESER == "",
    label = "non-serious adverse events"
  ) %>%
  define_parameter(
    name = "ser",
    subset = AESER == "Y",
    label = "serious adverse events"
  ) %>%
  define_parameter(
    name = "ser0rel",
    subset = AESER == "Y" & AEREL == "RELATED",
    label = "serious drug-related adverse events"
  ) %>%
  define_parameter(
    name = "dth",
    subset = AESDTH == "Y",
    label = "adverse events result in death"
  ) %>%
  define_parameter(
    name = "dtc0rel",
    subset = AESDTH == "Y" & AEREL == "RELATED",
    label = "drug-related adverse events result in death"
  ) %>%
  define_parameter(
    name = "disc",
    subset = toupper(AEACN) == "DRUG WITHDRAWN",
    label = "adverse events resulting in discontinuation"
  ) %>%
  define_parameter(
    name = "disc0drel",
    subset = toupper(AEACN) == "DRUG WITHDRAWN" & AEREL == "RELATED",
    label = "drug-related adverse events resulting in discontinuation"
  ) %>%
  define_parameter(
    name = "disc0ser",
    subset = toupper(AEACN) == "DRUG WITHDRAWN" & AESER == "Y",
    label = "serious adverse events resulting in discontinuation"
  ) %>%
  define_parameter(
    name = "disc0ser0rel",
    subset = toupper(AEACN) == "DRUG WITHDRAWN" & AESER == "Y" & AEREL == "RELATED",
    label = "serious drug-related adverse events resulting in discontinuation"
  )


meta <- meta %>%
  define_analysis(
    name = "ae_summary",
    title = "Adverse Event Summary"
  )


meta <- meta %>% meta_build()

meta$plan <- meta$plan %>%
  mutate(output_report = spec_filename(meta))

test_meta <- meta
# usethis::use_data(test_meta, overwrite = TRUE)

x <- prepare_ae_summary(
  meta = test_meta,
  population = "apat",
  observation = "trtemfl",
  parameter = sum_par
)


test_that("output from prepare_ae_summary is a list", {
  expect_true("outdata" %in% class(x))
})

xx <- tibble(cbind(x$name, x$n)) %>% mutate(x = row_number())
xxp <-x$prop %>% mutate(x = row_number())

adsl_tot <- adsl %>% mutate(TRT01AN = 99) # add total rows into calculation
adsl_tot <- rbind(adsl, adsl_tot)

res_tot <- adsl_tot %>%
  group_by(TRT01AN, ITTFL) %>%
  summarise(n = n_distinct(USUBJID), .groups = 'drop')

res_tot1 <- res_tot %>% mutate(z=n) %>% select(-c(n))


test_that("population count", {

  res <- res_tot %>%
    mutate(x = paste0("n_", row_number()))

  # res_total <- adsl_tot %>%
  #   group_by(TRT01A, ITTFL) %>%
  #   summarise(tot = n_distinct(USUBJID)) #total within each grp


  # res <- res %>%
  #   left_join(res_total, by = "TRT01A", "ITTFL") %>%
  #   mutate(pct = formatC(100*n/tot, digits = 0, format = "f", flag = "0")) #calculate pct
  #
  # dum_pct <- formatC(0,digits = decimal, format = "f", flag = "0")

  res1 <- res %>%
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  expect_equal(xx %>% filter(x == 1) %>% select(-c("x$name", x)), res1 %>% select(-c(ITTFL)))
  expect_equal(xx %>% filter(x == 1) %>% select(-c("x$name", x)), tibble(x$n_pop))
})

test_that("Parameter 'any' count", {
  adae_itt <- full_join(adsl %>% select(USUBJID, TRT01AN, ITTFL), # merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) %>% subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt %>% mutate(TRT01AN = 99) # add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot %>%
    group_by(TRT01AN, ITTFL) %>%
    summarise(n = n_distinct(USUBJID), .groups = 'drop')

  res_ae <- full_join(res_ae,res_tot1, by = c("TRT01AN", "ITTFL")) %>%
    mutate(pct = formatC(100*n/z, digits = 13, format = "f", flag = "0"))

  res_ae <- res_ae %>%
    mutate(x = paste0("n_", row_number()),
           prop=paste0("prop_", row_number()))

  res_ae2 <- res_ae %>%
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  res_prop2 <- res_ae %>%
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "prop",
      values_from = pct,
      values_fill = list(n = 0)
    )
  expect_equal(xx %>% filter(x == 2) %>% select(-c("x$name", x)), res_ae2 %>% select(-c(ITTFL)))
  expect_equal(xxp %>% filter(x == 2) %>% select(-c("x$name", x)), res_prop2 %>% select(-c(ITTFL)))
})


test_that("Parameter 'drug-related' count", {
  adae_itt <- full_join(adsl %>% select(USUBJID, TRT01AN, ITTFL), # merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) %>% subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt %>% mutate(TRT01AN = 99) # add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot %>%
    group_by(TRT01AN, ITTFL, AEREL) %>%
    summarise(n = n_distinct(USUBJID), .groups = 'drop')

  if (!"RELATED" %in% res_ae$AEREL) {
    res_ae <- res_ae %>%
      select(TRT01AN, ITTFL) %>%
      distinct() %>%
      mutate(AEREL = "RELATED", n = 0)
  }

  res_ae <- res_ae %>%
    mutate(x = paste0("n_", row_number()))

  res_ae3 <- res_ae %>%
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  expect_equal(xx %>% filter(x == 3) %>% select(-c("x$name", x)), res_ae3 %>% select(-c(ITTFL)))
})



test_that("Parameter 'non-serious' count", {
  adae_itt <- full_join(adsl %>% select(USUBJID, TRT01AN, ITTFL), # merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) %>% subset(ITTFL == "Y" & TRTEMFL == "Y" & AESER != "Y" | AESER == "")

  adae_tot <- adae_itt %>% mutate(TRT01AN = 99) # add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot %>%
    group_by(TRT01AN, ITTFL) %>%
    summarise(n = n_distinct(USUBJID), .groups = 'drop')

  res_ae <- res_ae %>%
    mutate(x = paste0("n_", row_number()))

  res_ae4 <- res_ae %>%
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  expect_equal(xx %>% filter(x == 4) %>% select(-c("x$name", x)), res_ae4 %>% select(-c(ITTFL)))
})


test_that("Parameter 'serious' count", {
  adae_itt <- full_join(adsl %>% select(USUBJID, TRT01AN, ITTFL), # merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) %>% subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt %>% mutate(TRT01AN = 99) # add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot %>%
    group_by(TRT01AN, ITTFL, AESER) %>%
    summarise(n = n_distinct(USUBJID), .groups = 'drop')

  res_ae_dummy <- res_ae %>%
    select(TRT01AN, ITTFL) %>%
    distinct() %>%
    mutate(AESER = "Y", t = as.integer(0))

  res_ae <- res_ae %>% subset(AESER == "Y")

  res_ae <- left_join(res_ae_dummy,
    res_ae,
    by = c("TRT01AN", "ITTFL", "AESER")
  ) %>% select(-c(t))

  res_ae[is.na(res_ae)] <- 0

  res_ae <- res_ae %>%
    mutate(x = paste0("n_", row_number()))

  res_ae5 <- res_ae %>%
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  expect_equal(xx %>% filter(x == 5) %>% select(-c("x$name", x)), res_ae5 %>% select(-c(ITTFL)))
})


test_that("Parameter 'serious and drug-related' count", {
  adae_itt <- full_join(adsl %>% select(USUBJID, TRT01AN, ITTFL), # merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) %>% subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt %>% mutate(TRT01AN = 99) # add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot %>%
    group_by(TRT01AN, ITTFL, AESER, AEREL) %>%
    summarise(n = n_distinct(USUBJID), .groups = 'drop')

  res_ae_dummy <- res_ae %>%
    ungroup() %>%
    select(TRT01AN, ITTFL) %>%
    distinct() %>%
    mutate(AESER = "Y", AEREL = "RELATED", t = as.integer(0))

  res_ae <- res_ae %>% subset(AESER == "Y" & AEREL == "RELATED")

  res_ae <- left_join(res_ae_dummy,
    res_ae,
    by = c("TRT01AN", "ITTFL", "AESER", "AEREL")
  ) %>% select(-c(t))

  res_ae[is.na(res_ae)] <- 0

  res_ae <- res_ae %>%
    mutate(x = paste0("n_", row_number()))

  res_ae6 <- res_ae %>%
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  expect_equal(xx %>% filter(x == 6) %>% select(-c("x$name", x)), res_ae6 %>% select(-c(ITTFL)))
})


test_that("Parameter 'death' count", {
  adae_itt <- full_join(adsl %>% select(USUBJID, TRT01AN, ITTFL), # merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) %>% subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt %>% mutate(TRT01AN = 99) # add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot %>%
    group_by(TRT01AN, ITTFL, AESDTH) %>%
    summarise(n = n_distinct(USUBJID), .groups = 'drop')

  res_ae_dummy <- res_ae %>%
    ungroup() %>%
    select(TRT01AN, ITTFL) %>%
    distinct() %>%
    mutate(AESDTH = "Y", t = as.integer(0))

  res_ae <- res_ae %>% subset(AESDTH == "Y")

  res_ae <- left_join(res_ae_dummy,
    res_ae,
    by = c("TRT01AN", "ITTFL", "AESDTH")
  ) %>% select(-c(t))

  res_ae[is.na(res_ae)] <- 0

  res_ae <- res_ae %>%
    mutate(x = paste0("n_", row_number()))

  res_ae7 <- res_ae %>%
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  expect_equal(xx %>% filter(x == 7) %>% select(-c("x$name", x)), res_ae7 %>% select(-c(ITTFL)))
})


test_that("Parameter 'death and drug-related' count", {
  adae_itt <- full_join(adsl %>% select(USUBJID, TRT01AN, ITTFL), # merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) %>% subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt %>% mutate(TRT01AN = 99) # add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot %>%
    group_by(TRT01AN, ITTFL, AESDTH, AEREL) %>%
    summarise(n = n_distinct(USUBJID), .groups = 'drop')

  res_ae_dummy <- res_ae %>%
    ungroup() %>%
    select(TRT01AN, ITTFL) %>%
    distinct() %>%
    mutate(AESDTH = "Y", AEREL = "RELATED", t = as.integer(0))

  res_ae <- res_ae %>% subset(AESDTH == "Y" & AEREL == "RELATED")

  res_ae <- left_join(res_ae_dummy,
    res_ae,
    by = c("TRT01AN", "ITTFL", "AESDTH", "AEREL")
  ) %>% select(-c(t))

  res_ae[is.na(res_ae)] <- 0

  res_ae <- res_ae %>%
    mutate(x = paste0("n_", row_number()))

  res_ae8 <- res_ae %>%
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  expect_equal(xx %>% filter(x == 8) %>% select(-c("x$name", x)), res_ae8 %>% select(-c(ITTFL)))
})

test_that("Parameter 'DRUG WITHDRAWN' count", {
  adae_itt <- full_join(adsl %>% select(USUBJID, TRT01AN, ITTFL), # merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) %>% subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt %>% mutate(TRT01AN = 99) # add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot %>%
    group_by(TRT01AN, ITTFL, AEACN) %>%
    summarise(n = n_distinct(USUBJID), .groups = 'drop')

  res_ae_dummy <- res_ae %>%
    ungroup() %>%
    select(TRT01AN, ITTFL) %>%
    distinct() %>%
    mutate(AEACN = "DRUG WITHDRAWN", t = as.integer(0))

  res_ae <- res_ae %>% subset(toupper(AEACN) == "DRUG WITHDRAWN")

  res_ae <- left_join(res_ae_dummy,
    res_ae,
    by = c("TRT01AN", "ITTFL", "AEACN")
  ) %>% select(-c(t))

  res_ae[is.na(res_ae)] <- 0

  res_ae <- res_ae %>%
    mutate(x = paste0("n_", row_number()))

  res_ae9 <- res_ae %>%
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  expect_equal(xx %>% filter(x == 9) %>% select(-c("x$name", x)), res_ae9 %>% select(-c(ITTFL)))
})


test_that("Parameter 'DRUG WITHDRAWN and Drug-related' count", {
  adae_itt <- full_join(adsl %>% select(USUBJID, TRT01AN, ITTFL), # merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) %>% subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt %>% mutate(TRT01AN = 99) # add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot %>%
    group_by(TRT01AN, ITTFL, AEACN, AEREL) %>%
    summarise(n = n_distinct(USUBJID), .groups = 'drop')

  res_ae_dummy <- res_ae %>%
    ungroup() %>%
    select(TRT01AN, ITTFL) %>%
    distinct() %>%
    mutate(AEACN = "DRUG WITHDRAWN", AEREL = "RELATED", t = as.integer(0))

  res_ae <- res_ae %>% subset(toupper(AEACN) == "DRUG WITHDRAWN" & AEREL == "RELATED")

  res_ae <- left_join(res_ae_dummy,
    res_ae,
    by = c("TRT01AN", "ITTFL", "AEACN", "AEREL")
  ) %>% select(-c(t))

  res_ae[is.na(res_ae)] <- 0

  res_ae <- res_ae %>%
    mutate(x = paste0("n_", row_number()))

  res_ae10 <- res_ae %>%
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  expect_equal(xx %>% filter(x == 10) %>% select(-c("x$name", x)), res_ae10 %>% select(-c(ITTFL)))
})



test_that("Parameter 'DRUG WITHDRAWN and Serious' count", {
  adae_itt <- full_join(adsl %>% select(USUBJID, TRT01AN, ITTFL), # merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) %>% subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt %>% mutate(TRT01AN = 99) # add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot %>%
    group_by(TRT01AN, ITTFL, AEACN, AESER) %>%
    summarise(n = n_distinct(USUBJID), .groups = 'drop')

  res_ae_dummy <- res_ae %>%
    ungroup() %>%
    select(TRT01AN, ITTFL) %>%
    distinct() %>%
    mutate(AEACN = "DRUG WITHDRAWN", AESER = "Y", t = as.integer(0))

  res_ae <- res_ae %>% subset(toupper(AEACN) == "DRUG WITHDRAWN" & AESER == "Y")

  res_ae <- left_join(res_ae_dummy,
    res_ae,
    by = c("TRT01AN", "ITTFL", "AEACN", "AESER")
  ) %>% select(-c(t))

  res_ae[is.na(res_ae)] <- 0

  res_ae <- res_ae %>%
    mutate(x = paste0("n_", row_number()))

  res_ae11 <- res_ae %>%
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  expect_equal(xx %>% filter(x == 11) %>% select(-c("x$name", x)), res_ae11 %>% select(-c(ITTFL)))
})

test_that("Parameter 'DRUG WITHDRAWN and Serious and Drug-related' count", {
  adae_itt <- full_join(adsl %>% select(USUBJID, TRT01AN, ITTFL), # merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) %>% subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt %>% mutate(TRT01AN = 99) # add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot %>%
    group_by(TRT01AN, ITTFL, AEACN, AESER, AEREL) %>%
    summarise(n = n_distinct(USUBJID), .groups = 'drop')

  res_ae_dummy <- res_ae %>%
    select(TRT01AN, ITTFL) %>%
    distinct() %>%
    mutate(AEACN = "DRUG WITHDRAWN", AESER = "Y", AEREL = "RELATED", t = as.integer(0))

  res_ae <- res_ae %>% subset(toupper(AEACN) == "DRUG WITHDRAWN" & AESER == "Y" & AEREL == "RELATED")

  res_ae <- left_join(res_ae_dummy,
    res_ae,
    by = c("TRT01AN", "ITTFL", "AEACN", "AESER", "AEREL")
  ) %>% select(-c(t))

  res_ae[is.na(res_ae)] <- 0

  res_ae <- res_ae %>%
    ungroup() %>%
    mutate(x = paste0("n_", row_number()))

  res_ae12 <- res_ae %>%
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  expect_equal(xx %>% filter(x == 12) %>% select(-c("x$name", x)), res_ae12 %>% select(-c(ITTFL)))
})

test_that("the reference group output of prepare_ae_summary match 1",{
  expect_equal(x$reference_group, 1)
})

#fin_ae <- rbind( res_ae2)