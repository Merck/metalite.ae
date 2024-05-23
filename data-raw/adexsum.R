# Read in ADEX dataset
load("data/metalite_ae_adex.rda")

adex <- metalite_ae_adex

adex1 <- adex |>
  dplyr::group_by(USUBJID) |>
  dplyr::mutate(
    AVAL = sum(EXDURDD, na.rm = TRUE),
    PARAM = "Duration of Exposure(days)",
    PARAMCD = "EXPODUR"
  ) |>
  dplyr::select(
    -VISIT, -VISITDY, -VISITNUM, -EXSTDTC, -EXENDTC, -EXENDY, -EXSTDY,
    -ASTDT, -AENDT, -ASTDTM, -AENDTM, -ASTDY, -AENDY, -EXDURDD
  ) |>
  dplyr::distinct(USUBJID, .keep_all = TRUE)

# Assign Label to derived variables
Hmisc::label(adex1$AVAL) <- "Analysis Value"
Hmisc::label(adex1$PARAM) <- "Parameter"
Hmisc::label(adex1$PARAMCD) <- "Parameter Code"

metalite_ae_adexsum <- adex1

usethis::use_data(metalite_ae_adexsum, overwrite = TRUE)
