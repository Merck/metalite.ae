meta <- meta_ae_dummy()
outdata <- prepare_ae_specific(meta,
                               population = "apat",
                               observation = "wk12",
                               parameter = "rel")
tbl <- outdata |>
  format_ae_specific()

tbl |>  tlf_ae_specific(
  medra_version = "24.0",
  source = "Source:  [CDISCpilot: adam-adsl; adae]",
  path_outtable = "outtable/ae0specific1.rtf"
)
