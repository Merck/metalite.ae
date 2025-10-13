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
#' meta <- meta_ae_example()
#' outdata <- prepare_ae_summary(meta,
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "any;rel;ser"
#' )
#' outdata |>
#'   format_ae_summary() |>
#'   gt_ae_summary(
#'     analysis = "ae_summary",
#'     source = "Source:  [CDISCpilot: adam-adsl; adae]"
#'   )
gt_ae_summary <- function(outdata,
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
  group <- outdata$group
  num_groups <- length(group)
  reference_group <- outdata$reference_group
  n_group <- length(outdata$group)
  n_row <- nrow(tbl)
  n_col <- ncol(tbl)

  analysis_name <- names(outdata$meta$analysis)
  if (!(analysis %in% analysis_name)) {
    stop(
      "Please provide a valid analysis that matches with what being defined in 'outdata$meta$analysis'",
      call. = FALSE
    )
  }

  parameters <- unlist(strsplit(outdata$parameter, ";"))

  if ("analysis" %in% title | "observation" %in% title | "population" %in% title) {
    title <- collect_title(outdata$meta,
      outdata$population,
      outdata$observation,
      "any",
      analysis = "ae_summary"
    )
    print(title)
  }

  x <- lapply(parameters, function(x) {
    collect_adam_mapping(outdata$meta, x)$summ_foot
  })
  footnotes <- c(unlist(x), footnotes)
  combined_title_md <- paste(title, collapse = "  \n")

  # Create spanner functions list
  spanner_funs <- lapply(seq_along(group), function(i) {
    function(gt_tbl) {
      gt_tbl %>% tab_spanner(
        label = group[i],
        columns = c(paste0("n_", i), paste0("prop_", i))
      )
    }
  })

  name_col <- colnames(tbl)[1] # first column, e.g. "name"
  n_cols <- paste0("n_", 1:num_groups)
  prop_cols <- paste0("prop_", 1:num_groups)

  cols_label_vec <- c(
    setNames("", name_col),
    setNames(rep("n", num_groups), n_cols),
    setNames(rep("(%)", num_groups), prop_cols)
  )

  # -------------------------
  # Helper: convert {^text} to <sup>text</sup>
  convert_caret_sup <- function(x) {
    if (is.null(x)) {
      return(x)
    }
    if (is.factor(x)) x <- as.character(x)
    # handle vector input (character vector)
    x_char <- as.character(x)
    na_idx <- is.na(x_char)

    # Escape existing angle brackets to avoid accidental HTML being interpreted,
    # then insert <sup> tags for {^...} patterns
    x_char <- gsub("<", "&lt;", x_char, fixed = TRUE)
    x_char <- gsub(">", "&gt;", x_char, fixed = TRUE)

    # Replace {^...} with <sup>...</sup>, allow multiple occurrences
    x_char <- gsub("\\{\\^([^}]+)\\}", "<sup>\\1</sup>", x_char, perl = TRUE)

    x_char[na_idx] <- NA_character_
    x_char
  }

  # Apply conversion to the name column (first column)
  if (name_col %in% colnames(tbl)) {
    tbl[[name_col]] <- convert_caret_sup(tbl[[name_col]])
  }

  # Convert footnotes vector and source string
  footnotes <- if (!is.null(footnotes)) gt::md(convert_caret_sup(footnotes)) else gt::md(footnotes)
  source <- if (!is.null(source)) gt::md(convert_caret_sup(source)) else gt::md(source)

  gt_tbl <- tbl |>
    gt() |>
    sub_missing(columns = 1:ncol(tbl), missing_text = "") |>
    fmt_markdown(columns = 1) |>
    tab_header(title = gt::md(combined_title_md)) |>
    (\(gt_tbl) Reduce(function(acc, f) f(acc), spanner_funs, init = gt_tbl))() |>
    cols_label(!!!cols_label_vec)

  # Add footnotes and source (if present). gt::tab_source_note accepts a character vector;
  # we pass the (possibly converted) footnotes and source.
  if (!is.null(footnotes) && length(footnotes) > 0) {
    gt_tbl <- gt_tbl %>% tab_source_note(footnotes)
  }
  if (!is.null(source) && nzchar(source)) {
    gt_tbl <- gt_tbl %>% tab_source_note(source)
  }

  return(gt_tbl)
}
