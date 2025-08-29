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
#'     footnotes = "Test",
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
  # analysis <- "ae_summary"
  # source = "Source:  [CDISCpilot: adam-adsl; adae]"

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
      "any",
      analysis = "ae_summary"
    )
    print(title)
  }

  # Footnotes
  x <- lapply(parameters, function(x) {
    collect_adam_mapping(outdata$meta, x)$summ_foot
  })
  footnotes <- c(unlist(x), footnotes)
  # footnotes <- paste(footnotes, collapse = "  \n")
  tbl
  combined_title_md <- paste(title, collapse = "  \n")

  # Function to create dynamic cols_label named vector
  create_cols_label <- function(num_groups) {
    n_labels <- setNames(rep("n", num_groups), paste0("n_", seq_len(num_groups)))
    prop_labels <- setNames(rep("(%)", num_groups), paste0("prop_", seq_len(num_groups)))
    c(name = "", n_labels, prop_labels)
  }

  # Create spanner functions list
  spanner_funs <- lapply(seq_along(group), function(i) {
    function(gt_tbl) {
      gt_tbl %>% tab_spanner(
        label = group[i],
        columns = c(paste0("n_", i), paste0("prop_", i))
      )
    }
  })

  n_labels <- setNames(rep("n", num_groups), paste0("n_", seq_len(num_groups)))
  prop_labels <- setNames(rep("(%)", num_groups), paste0("prop_", seq_len(num_groups)))

  name_col <- colnames(tbl)[1] # first column, e.g. "name"
  n_cols <- paste0("n_", 1:num_groups)
  prop_cols <- paste0("prop_", 1:num_groups)

  cols_label_vec <- c(
    setNames("", name_col), # first column label is empty string
    setNames(rep("n", num_groups), n_cols),
    setNames(rep("(%)", num_groups), prop_cols)
  )


  gt_tbl <- tbl |>
    gt() |>
    sub_missing(columns = 1:ncol(tbl), missing_text = "") |>
    fmt_markdown(columns = 1) |>
    tab_header(title = gt::md(combined_title_md)) |>
    (\(gt_tbl) Reduce(function(acc, f) f(acc), spanner_funs, init = gt_tbl))() |>
    cols_label(!!!cols_label_vec) |>
    tab_source_note(footnotes) |>
    tab_source_note(source)

  gtsave(gt_tbl, filename = "~/my_table.png")
}
