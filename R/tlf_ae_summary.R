# Copyright (c) 2023 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
# All rights reserved.
#
# This file is part of the metalite.ae program.
#
# metalite.ae is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' AE summary table
#'
#' @inheritParams tlf_ae_specific
#'
#' @param analysis One of analysis name existing at `outdata$meta$analysis`
#'
#' @return RTF file and the source dataset for AE summary table.
#'
#' @export
#'
#' @examples
#' # Define metadata
#' adsl <- forestly::forestly_adsl
#' adae <- forestly::forestly_adae
#'
#' adsl$TRT01A <- factor(
#'   adsl$TRT01A,
#'   levels = c("Xanomeline Low Dose", "Placebo"),
#'   labels = c("Low Dose", "Placebo")
#' )
#' adae$TRTA <- factor(
#'   adae$TRTA,
#'   levels = c("Xanomeline Low Dose", "Placebo"),
#'   labels = c("Low Dose", "Placebo")
#' )
#'
#' analysis_plan <- metalite::plan(
#'   analysis = "ae_summary",
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "any;rel;ser"
#' )
#'
#' meta <- metalite::meta_adam(observation = adae, population = adsl) |>
#'   metalite::define_plan(analysis_plan) |>
#'   metalite::define_population(
#'     name = "apat",
#'     var = c(
#'       "USUBJID", "SAFFL", "TRT01A", "TRTDUR",
#'       "SITEID", "SEX", "RACE", "AGE"
#'     ),
#'     group = "TRT01A",
#'     subset = SAFFL == "Y",
#'     label = "All Participants as Treated"
#'   ) |>
#'   metalite::define_observation(
#'     name = "wk12",
#'     var = c(
#'       "USUBJID", "SAFFL", "TRTA", "AEDECOD", "AEBODSYS", "AEREL",
#'       "AESER", "AEOUT", "AEACN", "AESDTH", "ASTDT", "AENDT"
#'     ),
#'     group = "TRTA",
#'     subset = SAFFL == "Y",
#'     label = "Weeks 0 to 12"
#'   ) |>
#'   metalite::define_parameter(
#'     name = "any",
#'     term1 = "",
#'     term2 = "",
#'     var = "AEDECOD",
#'     soc = "AEBODSYS",
#'     label = "All AEs"
#'   ) |>
#'   metalite::define_parameter(
#'     name = "rel",
#'     term1 = "Drug-Related",
#'     term2 = "",
#'     subset = AEREL %in% c("POSSIBLE", "PROBABLE"),
#'     var = "AEDECOD",
#'     soc = "AEBODSYS",
#'     label = "Drug-related AEs"
#'   ) |>
#'   metalite::define_parameter(
#'     name = "ser",
#'     term1 = "Serious",
#'     term2 = "",
#'     subset = AESER == "Y",
#'     var = "AEDECOD",
#'     soc = "AEBODSYS",
#'     label = "Serious AEs"
#'   ) |>
#'   metalite::define_analysis(
#'     name = "ae_summary",
#'     title = "Adverse Event Summary"
#'   ) |>
#'   metalite::meta_build()
#'
#' outdata <- prepare_ae_summary(meta,
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "any;rel;ser"
#' )
#' outdata |>
#'   format_ae_summary() |>
#'   tlf_ae_summary(
#'     source = "Source:  [CDISCpilot: adam-adsl; adae]",
#'     analysis = "ae_summary",
#'     path_outdata = tempfile(fileext = ".Rdata"),
#'     path_outtable = tempfile(fileext = ".rtf")
#'   )
tlf_ae_summary <- function(outdata,
                           source,
                           analysis,
                           col_rel_width = NULL,
                           text_font_size = 9,
                           orientation = "portrait",
                           title = c("analysis", "observation", "population"),
                           footnotes = NULL,
                           path_outdata = NULL,
                           path_outtable = NULL) {
  tbl <- outdata$tbl
  display <- outdata$display
  display_total <- "total" == display
  group <- outdata$group
  reference_group <- outdata$reference_group
  group_diff <- group[seq_along(group) != reference_group & group != "Total"]
  n_group <- length(outdata$group)
  n_group_diff <- length(group_diff)
  n_row <- nrow(tbl)
  n_col <- ncol(tbl)

  # Check if the parameter analysis contains the correct analysis that should exist in "outdata$meta$analysis"
  analysis_name <- names(outdata$meta$analysis)
  if (!(analysis %in% analysis_name)) {
    stop(
      "Please provide a valid analysis that matches with what being defined in 'outdata$meta$analysis'",
      call. = FALSE
    )
  }

  parameters <- unlist(strsplit(outdata$parameter, ";"))

  # Title
  # Define title
  if ("analysis" %in% title | "observation" %in% title | "population" %in% title) {
    title <- collect_title(outdata$meta,
      outdata$population,
      outdata$observation,
      parameters[1],
      analysis = analysis,
      title_order = title
    )
  }

  # Footnotes
  x <- lapply(parameters, function(x) {
    collect_adam_mapping(outdata$meta, x)$summ_foot
  })
  footnotes <- c(unlist(x), footnotes)

  if (!all(outdata$n_pop == 0)) {
    # Define column header
    col_tbl_within <- strsplit(names(tbl), "_") |>
      unlist() |>
      (\(list) list[list %in% c("n", "prop", "dur", "eventsavg", "eventscount")])() |>
      unique()

    colhead_within <- paste(
      vapply(
        X = col_tbl_within,
        FUN.VALUE = "character",
        FUN = switch,
        "n" = "n",
        "prop" = "(%)",
        "dur" = "Mean Duration (SE)",
        "eventsavg" = "Mean Events per Participant (SE)",
        "eventscount" = "Number of Events"
      ),
      collapse = " | "
    )

    colheader <- c(
      paste0(" | ", paste(group, collapse = " | ")),
      paste0(" | ", paste(rep(colhead_within, n_group), collapse = " | "))
    )

    rel_width_group <- rep(1, length(col_tbl_within) * n_group)
    rel_width <- c(3, rel_width_group)

    colborder_within <- vapply(
      X = col_tbl_within,
      FUN.VALUE = "character",
      FUN = switch,
      "n" = "single",
      "prop" = "",
      "dur" = "single",
      "eventsavg" = "single",
      "eventscount" = "",
      USE.NAMES = FALSE
    )

    border_left <- c(
      "single",
      rep(colborder_within, n_group)
    )

    # For CI and p-value with multiple groups following WMA mock up table.
    col_tbl_between <- strsplit(names(tbl), "_") |>
      unlist() |>
      (\(list) list[list %in% c("diff", "ci", "p")])() |>
      unique()

    if (length(col_tbl_between) > 0) {
      colhead_between <- paste(
        vapply(
          X = col_tbl_between,
          FUN.VALUE = "character",
          FUN = switch,
          "diff" = "Estimate",
          "ci" = paste0("(", outdata$ci_level * 100, "% CI)"),
          "p" = "p-value",
        ),
        collapse = " | "
      )

      if (n_group_diff == 1) {
        colheader_ci <- paste("Difference in % vs", group[reference_group])
      } else {
        colheader_ci <- paste0(paste("Difference in %", group_diff, "vs", group[reference_group]), collapse = " | ")
      }

      colheader_ci <- c(
        colheader_ci,
        paste(rep(colhead_between, n_group_diff), collapse = " | ")
      )

      colheader <- paste(colheader, colheader_ci, sep = " | ")

      rel_width_diff <- rep(1, length(col_tbl_between) * (n_group_diff))
      rel_width <- c(rel_width, rel_width_diff)

      colborder_between <- vapply(
        X = col_tbl_between,
        FUN.VALUE = "character",
        FUN = switch,
        "diff" = "single",
        "ci" = "",
        "p" = "single",
        USE.NAMES = FALSE
      )
      border_left <- c(
        border_left,
        rep(colborder_between, n_group_diff)
      )
    }

    # Relative width
    if (!is.null(col_rel_width)) {
      rel_width <- col_rel_width
    }

    n_col <- length(rel_width)

    rel_width1 <- c(
      rel_width[1],
      tapply(rel_width[2:(n_group * 2 + 1)], c(rep(1:n_group, each = 2)), sum)
    )

    if (length(col_tbl_between) > 0) {
      rel_width1 <- c(
        rel_width1,
        tapply(rel_width_diff, c(rep(1:n_group_diff, each = length(col_tbl_between))), sum)
      )
    }

    # Column boarder
    border_top <- c("", rep("single", n_col - 1))

    # Using order number to customize row format
    text_justification <- c("l", rep("c", n_col - 1))

    text_format <- ""
    text_format <- matrix(text_format, nrow = n_row, ncol = n_col)

    text_indent <- matrix(0, nrow = n_row, ncol = n_col)
    text_indent[, 1] <- ifelse(outdata$order == 1, 0, 100)

    # Use r2rtf
    outdata$rtf <- tbl |>
      r2rtf::rtf_page(orientation = orientation) |>
      r2rtf::rtf_title(title) |>
      r2rtf::rtf_colheader(
        colheader = colheader[1],
        col_rel_width = rel_width1,
        text_font_size = text_font_size
      ) |>
      r2rtf::rtf_colheader(
        colheader = colheader[2],
        border_top = border_top,
        border_left = border_left,
        col_rel_width = rel_width,
        text_font_size = text_font_size
      ) |>
      r2rtf::rtf_body(
        col_rel_width = rel_width,
        border_left = border_left,
        text_justification = text_justification,
        text_indent_first = text_indent,
        text_indent_left = text_indent,
        text_format = text_format,
        text_font_size = text_font_size
      )
  } else {
    outdata$rtf <- empty_table(
      title = title,
      orientation = orientation,
      text_font_size = text_font_size
    )
  }

  if (!is.null(footnotes)) {
    outdata$rtf <- outdata$rtf |>
      r2rtf::rtf_footnote(footnotes,
        text_font_size = text_font_size
      )
  }

  if (!is.null(source)) {
    outdata$rtf <- outdata$rtf |>
      r2rtf::rtf_source(source,
        text_font_size = text_font_size
      )
  }

  # Prepare output
  rtf_output(outdata, path_outdata, path_outtable)
}
