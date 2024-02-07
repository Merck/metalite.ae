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

#' Specific adverse events table
#'
#' @param outdata An `outdata` object created by [prepare_ae_specific()].
#' @param meddra_version A character value of the MedDRA version
#'   for this dataset.
#' @param source A character value of the data source.
#' @inheritParams r2rtf::rtf_page
#' @inheritParams r2rtf::rtf_body
#' @param footnotes A character vector of table footnotes.
#' @param title Term "analysis", "observation"and "population") for collecting
#'   title from metadata or a character vector of table titles.
#' @param path_outdata A character string of the outdata path.
#' @param path_outtable A character string of the outtable path.
#'
#' @return RTF file and the source dataset for AE specific table.
#'
#' @export
#'
#' @examples
#' meta <- meta_ae_example()
#'
#' meta |>
#'   prepare_ae_specific(
#'     population = "apat",
#'     observation = "wk12",
#'     parameter = "rel"
#'   ) |>
#'   format_ae_specific() |>
#'   tlf_ae_specific(
#'     source = "Source:  [CDISCpilot: adam-adsl; adae]",
#'     meddra_version = "24.0",
#'     path_outdata = tempfile(fileext = ".Rdata"),
#'     path_outtable = tempfile(fileext = ".rtf")
#'   )
tlf_ae_specific <- function(outdata,
                            meddra_version,
                            source,
                            col_rel_width = NULL,
                            text_font_size = 9,
                            orientation = "portrait",
                            footnotes = NULL,
                            title = c("analysis", "observation", "population"),
                            path_outdata = NULL,
                            path_outtable = NULL) {
  if (is.null(footnotes)) {
    footnotes <- c(
      "Every participant is counted a single time for each applicable row and column.",
      paste(
        "A system organ class or specific adverse event appears on this report only if",
        "its incidence in one or more of the columns meets the incidence",
        "criterion in the report title, after rounding."
      ),
      "Adverse event terms are from MedDRA Version {meddra_version}."
    )
  }

  tbl <- outdata$tbl
  group <- outdata$group
  reference_group <- outdata$reference_group
  display_total <- "Total" %in% group
  n_group <- length(outdata$group)
  between_total <- seq(n_group - (display_total))
  n_comparisons <- length(between_total) - 1
  n_row <- nrow(tbl)
  n_col <- ncol(tbl)

  if (!is.null(col_rel_width) && !(n_col == length(col_rel_width))) {
    stop(
      "col_rel_width must have the same length (has ",
      length(col_rel_width),
      ") as as `outdata$tbl` has number of columns (has ",
      n_col, ").",
      call. = FALSE
    )
  }

  # Define title
  if ("analysis" %in% title | "observation" %in% title | "population" %in% title) {
    title <- collect_title(outdata$meta,
      outdata$population,
      outdata$observation,
      outdata$parameter,
      analysis = "ae_specific",
      title_order = title
    )
  }

  footnotes <- vapply(footnotes, glue::glue_data,
    .x = list(meddra_version = meddra_version), FUN.VALUE = character(1)
  )
  names(footnotes) <- NULL

  # Within (group statistics)
  col_tbl_within <- strsplit(names(tbl), "_") |>
    unlist() |>
    (\(list) list[list %in% c("n", "prop", "dur", "events")])() |>
    unique()

  colhead_within <- paste(
    vapply(
      X = col_tbl_within,
      FUN.VALUE = "character",
      FUN = switch,
      "n" = "n",
      "prop" = "(%)",
      "dur" = "Mean Duration (SE)",
      "events" = "Mean Events per Participant (SE)"
    ),
    collapse = " | "
  )

  colhead_1_within <- paste(group, collapse = " | ")
  colhead_2_within <- paste(rep(colhead_within, n_group), collapse = " | ")

  colborder_within <- vapply(
    X = col_tbl_within,
    FUN.VALUE = "character",
    FUN = switch,
    "n" = "single",
    "prop" = "",
    "dur" = "single",
    "events" = "single",
    USE.NAMES = FALSE
  )

  rwidth_2_within <- rep(1, length(col_tbl_within) * n_group)

  rwidth_1_within <- tapply(
    rwidth_2_within,
    c(rep(1:n_group, each = length(col_tbl_within))),
    sum
  )

  colborder_within <- rep(colborder_within, n_group)

  # Between (comparison statistics)
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

    colhead_1_between <- paste("Difference in %",
      outdata$group[between_total[-reference_group]],
      "vs.",
      outdata$group[reference_group],
      collapse = " | "
    )

    colhead_2_between <- paste(rep(colhead_between, n_comparisons), collapse = " | ")

    colborder_between <- vapply(
      X = col_tbl_between,
      FUN.VALUE = "character",
      FUN = switch,
      "diff" = "single",
      "ci" = "",
      "p" = "single",
      USE.NAMES = FALSE
    )

    rwidth_2_between <- rep(1, length(col_tbl_between) * n_comparisons)

    rwidth_1_between <- tapply(
      rwidth_2_between,
      c(rep(1:n_comparisons, each = length(col_tbl_between))),
      sum
    )

    colborder_between <- rep(colborder_between, n_comparisons)
  } else {
    colhead_between <- colhead_1_between <- colhead_2_between <- NULL
    rwidth_1_between <- rwidth_2_between <- colborder_between <- NULL
  }

  # Column headers
  colheader <- c(
    paste0(" | ", paste0(c(colhead_1_within, colhead_1_between), collapse = " | ")),
    paste0(" | ", paste0(c(colhead_2_within, colhead_2_between), collapse = " | "))
  )

  # Relative width
  if (is.null(col_rel_width)) {
    rwidth_2 <- c(3, rwidth_2_within, rwidth_2_between)
    rwidth_1 <- c(3, rwidth_1_within, rwidth_1_between)
  } else {
    rwidth_2 <- col_rel_width

    rw_1_recalc_w <- tapply(
      col_rel_width[2:(n_group * length(col_tbl_within) + 1)],
      c(rep(1:n_group, each = length(col_tbl_within))), sum
    )

    rw_1_recalc_b <- if (length(col_tbl_between) > 0) {
      tapply(
        col_rel_width[(n_group * length(col_tbl_within) + 2):n_col],
        c(rep(1:n_comparisons, each = length(col_tbl_between))), sum
      )
    } else {
      NULL
    }

    rwidth_1 <- c(
      rwidth_2[1],
      rw_1_recalc_w,
      rw_1_recalc_b
    )
  }

  if (sum(rwidth_1) != sum(rwidth_2)) {
    stop("Width calculation breaks, please contact developer.", call. = FALSE)
  }

  # Column border
  border_top <- c("", rep("single", n_col - 1))
  border_left <- c("single", colborder_within, colborder_between)

  # Use order number to customize row format
  text_justification <- c("l", rep("c", n_col - 1))

  if (length(outdata$components) == 2) {
    text_format <- ifelse(outdata$order %% 1000 == 0, "b", "")
  } else {
    text_format <- ""
  }
  text_format <- matrix(text_format, nrow = n_row, ncol = n_col)

  text_indent <- matrix(0, nrow = n_row, ncol = n_col)
  text_indent[, 1] <- ifelse(outdata$order %% 1000 == 0 | outdata$order == 1, 0, 100)

  # Use r2rtf
  outdata$rtf <- tbl |>
    r2rtf::rtf_page(orientation = orientation) |>
    r2rtf::rtf_title(title) |>
    r2rtf::rtf_colheader(
      colheader = colheader[1],
      col_rel_width = rwidth_1,
      text_font_size = text_font_size
    ) |>
    r2rtf::rtf_colheader(
      colheader = colheader[2],
      border_top = border_top,
      border_left = border_left,
      col_rel_width = rwidth_2,
      text_font_size = text_font_size
    ) |>
    r2rtf::rtf_body(
      col_rel_width = rwidth_2,
      border_left = border_left,
      text_justification = text_justification,
      text_indent_first = text_indent,
      text_indent_left = text_indent,
      text_format = text_format,
      text_font_size = text_font_size
    )

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
