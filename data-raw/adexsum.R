library(haven)
library(dplyr)
library(r2rtf)
library(Hmisc)

# Read in ADEX dataset
load("~/metalite.ae1/data/metalite_ae_adex.rda")

adex <- metalite_ae_adex

adex1 <- adex %>%
  group_by(USUBJID) %>%
  mutate(
    AVAL = sum(EXDURDD, na.rm = TRUE),
    PARAM = "Duration of Exposure(days)",
    PARAMCD = "EXPODUR"
  ) %>%
  select(-VISIT, -VISITDY, -VISITNUM, -EXSTDTC, -EXENDTC, -EXENDY, -EXSTDY, -ASTDT, -AENDT, -ASTDTM, -AENDTM, -ASTDY, -AENDY, -EXDURDD) %>%
  distinct(USUBJID, .keep_all = TRUE)

# Assign Label to derived variables
label(adex1$AVAL) <- "Analysis Value"
label(adex1$PARAM) <- "Parameter"
label(adex1$PARAMCD) <- "Parameter Code"

metalite_ae_adexsum <- adex1

usethis::use_data(metalite_ae_adexsum, overwrite = TRUE)
