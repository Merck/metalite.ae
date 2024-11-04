meta <- meta_ae_example()

meta


meta$data_observation <- meta$data_observation[0, ]

meta$data_population <- meta$data_population[0, ]

meta

# Prepare AE-specific output
outdata <- prepare_ae_specific(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel"
)

# Define the function to check for empty data and perform appropriate action
generate_analysis_or_empty <- function(meta, outdata) {
  if (nrow(meta$data_population) == 0 || nrow(meta$data_observation) == 0) {
    cat("Generating empty table due to zero observations...\n")
    empty_table_output <- empty_table(
      title = "Participants With Adverse Events",
      orientation = "portrait",
      text_font_size = 8
    )
    return(empty_table_output)
  } else {
    cat("Generating analysis table with `tlf_ae_specific`...\n")
    path_rtf <- file.path(tempdir(), "ae0emptytable1.rtf")
    path_rdata <- tempfile(fileext = ".Rdata")

    tbl <- outdata |>
      format_ae_specific(
        display_col = c("n", "prop"),
        digits_prop = 4
      ) |>
      tlf_ae_specific(
        source = "Source: [CDISCpilot: adam-adsl; adae]",
        analysis = "ae_specific",
        meddra_version = "24.0",
        path_outdata = path_rdata,
        path_outtable = path_rtf
      )

    return(tbl)
  }
}

# Execute the function
result <- generate_analysis_or_empty(meta, outdata)

# Test case to validate implementation
test_that("RTF output: empty table or analysis table.", {
  path_rtf <- file.path(tempdir(), "ae0emptytable2.rtf")
  path_rdata <- tempfile(fileext = ".Rdata")

  if (nrow(meta$data_population) == 0 || nrow(meta$data_observation) == 0) {
    empty_table_output <- empty_table(
      title = "Participants With Adverse Events",
      orientation = "portrait",
      text_font_size = 8
    )

    # Save the empty table to a file for snapshot testing
    writeLines(capture.output(empty_table_output), path_rtf)
    expect_snapshot_file(path_rtf)
  } else {
    tbl <- outdata |>
      format_ae_specific(
        display_col = c("n", "prop"),
        digits_prop = 4
      ) |>
      tlf_ae_specific(
        source = "Source: [CDISCpilot: adam-adsl; adae]",
        analysis = "ae_specific",
        meddra_version = "24.0",
        path_outdata = path_rdata,
        path_outtable = path_rtf
      )

    expect_true(file.exists(path_rtf))
    expect_snapshot_file(path_rtf)
  }
})
