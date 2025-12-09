# Read in ADEX dataset
load("data/metalite_ae_adex.rda")
adex <- metalite_ae_adex

# Summarize exposure duration
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

# using attr() to assign labels
attr(adex1$AVAL, "label") <- "Analysis Value"
attr(adex1$PARAM, "label") <- "Parameter"
attr(adex1$PARAMCD, "label") <- "Parameter Code"

adex1$EXDURDDU <- structure(adex1$EXDURDDU, label = "New Label for EXDURDDU")

# get rid of class labelled made with haven, Hmisc package
class(adex1$EXDURDDU) <- NULL
class(adex1$EXNUMDOS) <- NULL

attr(adex1$EXDURDDU, "label") <- "Exposure Duration Unit"
attr(adex1$EXNUMDOS, "label") <- "Number of Daily Doses"


# --------------------------------------------------
# New code: Simulated treatment duration dataset
# --------------------------------------------------

# The ADaM dataset for Drug Exposrue Summary Data, is utilized to:
#
#   - Sum up duration by STUDYID SITENUM USUBJID SUBJID APERIOD EXTRT ADOSEFRM PARAMCD.
#   - Subset the exposure data by `upcase(trim(left(paramcd))) = "TRTDUR"`.
#   - Get the exposure duration `adexsum$AVAL` for all participants.
#   - Assign duration category `adexsum$EXDURGR` i.e.">=1 day", ">=7 days",">=28 days", ">=12 weeks" and ">=24 weeks".

set.seed(123) # For reproducibility, keeping the rest of the code unchanged

adexsum <- r2rtf::r2rtf_adsl |>
  dplyr::select(USUBJID, TRT01A, TRT01P, AGE, AGEU, AGEGR1, SEX, RACE, RACEN, TRTSDT, SAFFL) |>
  dplyr::mutate(
    APERIODC = "Base",
    APERIOD = 1,
    PARQUAL = "All",
    PARAM = "Treatment Duration Actual in Days",
    PARAMCD = "TRTDURD",
    AVAL = sample(x = 0:(24 * 7), size = dplyr::n(), replace = TRUE),
    EXDURGR = dplyr::case_when(
      AVAL >= 24 * 7 ~ ">=24 weeks",
      AVAL >= 12 * 7 ~ ">=12 weeks",
      AVAL >= 28 ~ ">=28 days",
      AVAL >= 7 ~ ">=7 days",
      AVAL >= 1 ~ ">=1 day"
    )
  )

adexsum$EXDURGR <- factor(adexsum$EXDURGR,
  levels = c("not treated", ">=1 day", ">=7 days", ">=28 days", ">=12 weeks", ">=24 weeks")
)

adexsum$TRTA <- factor(adexsum$TRT01A,
  levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
  labels = c("Placebo", "Low Dose", "High Dose")
)

# To combine both ADEX1 and ADEXSUM, need to handle the following:
# Ungrouped grouped data frames
# Normalized only numeric labelled columns
# Preserved character-labelled columns as character
# Added missing columns without overwriting existing ones (some vars in ADEX1 only and some in ADEXSUM only)

library(dplyr)

safe_bind_rows <- function(df1, df2) {
  # Remove grouping
  df1 <- dplyr::ungroup(df1)

  # Normalize labelled columns (strip class, keep label attribute)
  normalize_labelled <- function(df) {
    df[] <- lapply(df, function(x) {
      if (inherits(x, "labelled")) {
        base <- unclass(x) # remove haven_labelled class
        lbl <- attr(x, "label", exact = TRUE)

        # Preserve type and label
        if (is.numeric(base)) {
          base <- as.numeric(base)
        } else {
          base <- as.character(base)
        }
        if (!is.null(lbl)) attr(base, "label") <- lbl
        base
      } else {
        x
      }
    })
    df
  }

  # Apply normalization
  df1 <- normalize_labelled(df1)
  df2 <- normalize_labelled(df2)

  # Align columns without overwriting
  all_cols <- union(names(df1), names(df2))
  for (col in all_cols) {
    if (!col %in% names(df1)) df1[[col]] <- NA
    if (!col %in% names(df2)) df2[[col]] <- NA
  }

  # Bind rows safely
  dplyr::bind_rows(df1, df2)
}

# Use the function
metalite_ae_adexsum <- safe_bind_rows(adex1, adexsum)

# Save the extended dataset
usethis::use_data(metalite_ae_adexsum, overwrite = TRUE)
