#test_that("multiplication works", {
 # est plan:

  #  The output is a list
  #Test each component
  #population: match the expectation
  #parameter: match the expectation
  # n: match that from SAS (take r2rtf::r2rtf_adsl and r2rtf::r2rtf_adae as an example)
  # order: match the expectation
  # group: match the expectation
  # reference_group: match the expectation
  # prop: match that from SAS (take r2rtf::r2rtf_adsl and r2rtf::r2rtf_adae as an example)
  # diff: match that from SAS (take r2rtf::r2rtf_adsl and r2rtf::r2rtf_adae as an example)
  # name: match the expectation
# })


devtools::load_all()
library(metalite)
library(haven)
library(dplyr)
library(tidyr)

# ---------------- #
#   read data      #
# ---------------- #
adsl <- r2rtf::r2rtf_adsl
adae <- r2rtf::r2rtf_adae %>% rename(TRT01A = TRTA)

# factor the grouping variables
adsl$TRT01P <- factor(adsl$TRT01P, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
adsl$TRT01A <- factor(adsl$TRT01A, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
adae$TRT01A <- factor(adae$TRT01A, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))

# ---------------- #
#   define meta    #
# ---------------- #
# define the observation and population
meta <- meta_adam(observation = adae,
                  population = adsl)

# define the AE summary analysis plan
sum_par <- "any;rel;nonser;ser;ser0rel;dth;dtc0rel;disc;disc0drel;disc0ser;disc0ser0rel"
plan <- plan(analysis = "ae_summary",
             population = "apat",
             observation = "trtemfl",
             parameter = sum_par,
             pilot = TRUE)

plan_pilot <- plan |>
  subset(pilot) |>
  mutate(display_total = analysis %in% c("ae_summary"))

meta <- meta |>
  define_plan(plan_pilot)

meta <- meta |>
  define_population(name = "apat",
                    group = "TRT01A",
                    subset = ITTFL == "Y",
                    label = "(All Participants as Treated)")

meta <- meta |>
  define_observation(name = "trtemfl",
                     group = "TRT01A",
                     var = "AEDECOD",
                     subset = TRTEMFL=="Y",
                     label = "Treatment emergent")

# define parameters
meta <- meta |>
  define_parameter(name = "any") |>
  define_parameter(name = "rel",
                   subset = AEREL %in% c("RELATED"),
                   label = "drug-related adverse events") |>
  define_parameter(name = "nonser",
                   subset = AESER != "Y" | AESER == "" ,
                   label = "non-serious adverse events") |>
  define_parameter(name = "ser",
                   subset = AESER == "Y",
                   label = "serious adverse events") |>
  define_parameter(name = "ser0rel",
                   subset = AESER == "Y" & AEREL %in% c("RELATED"),
                   label = "serious drug-related adverse events") |>
  define_parameter(name = "dth",
                   subset = AESDTH == "Y",
                   label = "adverse events result in death") |>
  define_parameter(name = "dtc0rel",
                   subset = AESDTH == "Y" & AEREL %in% c("RELATED"),
                   label = "drug-related adverse events result in death") |>
  define_parameter(name = "disc",
                   subset = toupper(AEACN) == 'DRUG WITHDRAWN',
                   label = "adverse events resulting in discontinuation") |>
  define_parameter(name = "disc0drel",
                   subset = toupper(AEACN) == 'DRUG WITHDRAWN' & AEREL %in% c("RELATED") ,
                   label = "drug-related adverse events resulting in discontinuation") |>
  define_parameter(name = "disc0ser",
                   subset = toupper(AEACN) == 'DRUG WITHDRAWN' & AESER == "Y" ,
                   label = "serious adverse events resulting in discontinuation") |>
  define_parameter(name = "disc0ser0rel",
                   subset = toupper(AEACN) == 'DRUG WITHDRAWN' & AESER == "Y" & AEREL %in% c("RELATED"),
                   label = "serious drug-related adverse events resulting in discontinuation")

# define analysis
meta <- meta |>
  define_analysis(name = "ae_summary",
                  title = "Adverse Event Summary")

# ---------------- #
#   build meta     #
# ---------------- #
meta <- meta %>% meta_build()

meta$plan <- meta$plan |>
  mutate(output_report = spec_filename(meta))

test_meta <- meta
#usethis::use_data(test_meta, overwrite = TRUE)

prepare_ae_summary(meta = test_meta,
                   population = "apat",
                   observation = "trtemfl",
                   parameter = sum_par)
