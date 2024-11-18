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

#' Specific adverse events table for subgroup analysis
#'
#' @inheritParams tlf_ae_specific
#'
#' @param analysis One of analysis name existing at `outdata$meta$analysis`
#'
#' @return RTF file and the source dataset for AE specific subgroup analysis table.
#'
#' @export
#'
#' @examples
#' meta <- meta_ae_example()
#' prepare_ae_specific_subgroup(meta,
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "rel",
#'   subgroup_var = "SEX",
#'   display_subgroup_total = TRUE
#' ) |>
#'   format_ae_specific_subgroup() |>
#'   tlf_ae_specific_subgroup(
#'     meddra_version = "24.0",
#'     source = "Source:  [CDISCpilot: adam-adsl; adae]",
#'     analysis = "ae_specific",
#'     path_outtable = tempfile(fileext = ".rtf")
#'   )
tlf_ae_specific_subgroup <- function(
    outdata,
    meddra_version,
    source,
    analysis,
    col_rel_width = NULL,
    text_font_size = 9,
    orientation = "landscape",
    footnotes = NULL,
    title = NULL,
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

  footnotes <- vapply(
    footnotes, glue::glue_data,
    .x = list(meddra_version = meddra_version), FUN.VALUE = character(1)
  )
  names(footnotes) <- NULL

  out_all <- outdata$out_all
  tbl <- outdata$tbl
  tbl1 <- tbl[names(tbl) != "order"]
  tgroup <- outdata$group
  sgroup <- outdata$subgroup
  if (outdata$display_subgroup_total) sgroup <- c(sgroup, "Total")
  n_sgroup <- length(sgroup)
  n_tgroup <- length(outdata$group)
  n_row <- nrow(tbl1)
  n_col <- ncol(tbl1)

  if (!is.null(col_rel_width) && !n_col == length(col_rel_width)) {
    stop(
      "col_rel_width must have the same length (has ",
      length(col_rel_width),
      ") as as outdata$tbl has number of columns (has ",
      n_col, ")."
    )
  }

  # Check if the parameter analysis contains the correct analysis that should exist in "outdata$meta$analysis"
  analysis_name <- names(outdata$meta$analysis)
  if (!(analysis %in% analysis_name)) {
    stop(
      "Please provide a valid analysis that matches with what being defined in 'outdata$meta$analysis'",
      call. = FALSE
    )
  }

  # Define title
  if (is.null(title)) {
    title <- collect_title(outdata$meta,
      outdata$population,
      outdata$observation,
      outdata$parameter,
      analysis = analysis
    )
  }

  if (!all(sapply(out_all, function(x) {
    all(x$n_pop == 0)
  }))) {
    col_tbl_within <- outdata$display

    col_tbl_within <- col_tbl_within |>
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

    colhead_1_within <- paste(sgroup, collapse = " |")
    colhead_2_within <- paste(rep(tgroup, n_sgroup), collapse = " | ")
    colhead_3_within <- paste(rep(colhead_within, n_sgroup * n_tgroup), collapse = " | ")

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

    rwidth_3_within <- rep(1, length(col_tbl_within) * n_sgroup * n_tgroup)

    rwidth_2_within <- tapply(
      rwidth_3_within,
      c(rep(1:(n_sgroup * n_tgroup), each = length(col_tbl_within))),
      sum
    )
    names(rwidth_2_within) <- NULL

    rwidth_1_within <- tapply(
      rwidth_3_within,
      c(rep(1:n_sgroup, each = length(col_tbl_within) * n_tgroup)),
      sum
    )
    names(rwidth_1_within) <- NULL


    colborder_within <- rep(colborder_within, n_sgroup * n_tgroup)

    # Column headers

    colheader <- c(
      paste0(" | ", colhead_1_within),
      paste0(" | ", colhead_2_within),
      paste0(" | ", colhead_3_within)
    )

    # Relative width

    if (is.null(col_rel_width)) {
      rwidth_1 <- c(2.5, rwidth_1_within)
      rwidth_2 <- c(2.5, rwidth_2_within)
      rwidth_3 <- c(2.5, rwidth_3_within)
    } else {
      rwidth_3 <- col_rel_width

      rwidth_2 <- tapply(
        col_rel_width[2:length(col_rel_width)],
        c(rep(1:(n_sgroup * n_tgroup), each = length(col_tbl_within))),
        sum
      )

      rwidth_2 <- c(
        rwidth_3[1],
        rwidth_2
      )


      rwidth_1 <- tapply(
        col_rel_width[2:length(col_rel_width)],
        c(rep(1:n_sgroup, each = length(col_tbl_within) * n_tgroup)),
        sum
      )

      rwidth_1 <- c(
        rwidth_3[1],
        rwidth_1
      )
    }

    if ((sum(rwidth_1) != sum(rwidth_2)) || (sum(rwidth_1) != sum(rwidth_3))) {
      stop("Width calculation breaks, contact developer.")
    }

    # Column border
    border_top2 <- c("", rep("single", n_sgroup * n_tgroup))
    border_top3 <- c("", rep("single", n_sgroup * n_tgroup * 2))

    border_left2 <- c("single", rep("single", n_sgroup * n_tgroup))
    border_left3 <- c("single", colborder_within)

    # Using order number to customize row format

    text_justification <- c("l", rep("c", n_sgroup * n_tgroup * 2))

    if (length(outdata$components) == 2) {
      text_format <- ifelse(tbl$order %% 1000 == 0, "b", "")
    } else {
      text_format <- ""
    }

    text_format <- matrix(text_format, nrow = n_row, ncol = n_col)

    text_indent <- matrix(0, nrow = n_row, ncol = n_col)
    text_indent[, 1] <- ifelse(tbl$order %% 1000 == 0 | tbl$order == 1, 0, 100)

    # Using r2rtf

    outdata$rtf <- tbl1 |>
      r2rtf::rtf_page(orientation = orientation) |>
      r2rtf::rtf_title(title) |>
      r2rtf::rtf_colheader(colheader = colheader[1], col_rel_width = rwidth_1, text_font_size = text_font_size) |>
      r2rtf::rtf_colheader(
        colheader = colheader[2], border_top = border_top2, border_left = border_left2,
        col_rel_width = rwidth_2, text_font_size = text_font_size
      ) |>
      r2rtf::rtf_colheader(
        colheader = colheader[3], border_top = border_top3, border_left = border_left3,
        col_rel_width = rwidth_3, text_font_size = text_font_size
      ) |>
      r2rtf::rtf_body(
        col_rel_width = rwidth_3, border_left = border_left3, text_justification = text_justification,
        text_indent_first = text_indent, text_indent_left = text_indent, text_format = text_format,
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
