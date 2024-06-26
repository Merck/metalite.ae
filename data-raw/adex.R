# Read in the ADSL and EX datasets
ex0 <- haven::read_xpt("data-raw/ex.xpt")
write.csv(ex0, file = "data-raw/ex.csv", row.names = FALSE, quote = FALSE)

# Create a new data object with selected variables
adsl <- r2rtf::r2rtf_adsl
adsl <- dplyr::select(
  adsl, STUDYID, SITEID, USUBJID, starts_with("TRT01"), AGE,
  starts_with("AGEGR1"),
  starts_with("RACE"), SEX, SAFFL, ITTFL, TRTSDT, TRTEDT
)

# Merge both dataset by usubjid
ex1 <- merge(adsl, ex0, by = c("STUDYID", "USUBJID"))

# Derive ASTDT and AENDT
adex1 <- ex1 |>
  dplyr::mutate(ASTDT = as.Date(EXSTDTC, format = "%Y-%m-%d")) |>
  dplyr::mutate(AENDT = as.Date(EXENDTC, format = "%Y-%m-%d"))

# ASTDTM and AENDTM
adex1$ASTDTM <- ifelse(
  nchar(adex1$EXSTDTC) == 10,
  paste(adex1$EXSTDTC, "T00:00:00"),
  EXSTDTC
)

adex1$AENDTM <- ifelse(
  nchar(adex1$EXENDTC) == 10,
  paste(adex1$EXENDTC, "T00:00:00"),
  adex1$EXENDTC
)

# Derive ASTDY AENDY EXDURDD EXDURDDU
adex2 <- adex1 |>
  dplyr::mutate(ASTDY = ASTDT - TRTSDT + (ASTDT >= TRTSDT)) |>
  dplyr::mutate(AENDY = AENDT - TRTSDT + (AENDT >= TRTSDT)) |>
  dplyr::mutate(EXDURDD = AENDT - ASTDT + (AENDT >= ASTDT)) |>
  dplyr::mutate(EXDURDDU = "Days")

# Convert ASTDY AENDY and EXDURDD into numeric
adex3 <- adex2 |>
  dplyr::mutate(ASTDY = as.numeric(adex2$ASTDY)) |>
  dplyr::mutate(AENDY = as.numeric(adex2$AENDY)) |>
  dplyr::mutate(EXDURDD = as.numeric(adex2$EXDURDD))

# Derive EXNUMDOS: If EXDOSE ge 0 then 1 else 0
adex3$EXNUMDOS <- ifelse(adex3$EXDOSE >= 0, 1, 0)

# Assign Label to derived variables
Hmisc::label(adex3$ASTDT) <- "Analysis Start Date"
Hmisc::label(adex3$AENDT) <- "Analysis End Date"
Hmisc::label(adex3$ASTDTM) <- "Analysis Start Date Time"
Hmisc::label(adex3$AENDTM) <- "Analysis End Date Time"
Hmisc::label(adex3$ASTDY) <- "Analysis Study Day"
Hmisc::label(adex3$AENDY) <- "Analysis End Day"
Hmisc::label(adex3$EXDURDD) <- "Exposure Duration"
Hmisc::label(adex3$EXDURDDU) <- "Exposure Duration Unit"
Hmisc::label(adex3$EXNUMDOS) <- "Number of Daily Doses"

metalite_ae_adex <- adex3

usethis::use_data(metalite_ae_adex, overwrite = TRUE)
