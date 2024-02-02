# Copyright (c) 2024 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
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

#' Exposure-adjusted AE summary table
#'
#' @inheritParams tlf_ae_specific
#'
#' @return RTF file and source dataset for exposure-adjusted AE summary table.
#'
#' @export
#'
#' @examples
#' meta <- meta_ae_example()
#' outdata <- meta |>
#'   prepare_ae_summary(
#'     population = "apat",
#'     observation = "wk12",
#'     parameter = "any;rel;ser"
#'   ) |>
#'   extend_ae_summary_eaer(adj_unit = "month")
#' outdata |>
#'   format_ae_exp_adj() |>
#'   tlf_ae_exp_adj(
#'     source = "Source:  [CDISCpilot: adam-adsl; adae]",
#'     path_outdata = tempfile(fileext = ".Rdata"),
#'     path_outtable = tempfile(fileext = ".rtf")
#'   )

tlf_ae_exp_adj <- function(outdata,
                           source,
                           col_rel_width = NULL,
                           text_font_size = 9,
                           orientation = "portrait",
                           title = c("analysis", "observation", "population"),
                           footnotes = NULL,
                           path_outdata = NULL,
                           path_outtable = NULL) {

  tbl <- outdata$tbl
  group <- outdata$group
  time_unit <- tolower(outdata$adj_unit)
  n_group <- length(outdata$group)
  n_row <- nrow(tbl)
  n_col <- ncol(tbl)

  parameters <- unlist(strsplit(outdata$parameter, ";"))

  # Title
  # Define title
  if ("analysis" %in% title | "observation" %in% title | "population" %in% title) {
    title <- collect_title(outdata$meta,
                           outdata$population,
                           outdata$observation,
                           parameters[1],
                           analysis = "ae_exp_adj",
                           title_order = title
    )
  }

  # Footnotes
  footnote <-
    paste0("{^a} Event rate per 100 person-",
           time_unit,
           " of exposure = event count *100/person-",
           time_unit,
           " of exposure.")
  if ("rel" %in% parameters){
    footnote <- paste(footnote,
                      "{^b} Determined by the investigator to be related to the drug.",
                      sep = "\n")
  }

  if (!is.null(footnotes)){
    footnotes <- paste(footnote, footnotes, sep = "\n")
  } else {
    footnotes <- paste(footnote, sep = "\n")
  }

  # !!! Need to check: Footnote is included in meta or defined as above  !!!
  # x <- lapply(parameters, function(x) {
  #   collect_adam_mapping(outdata$meta, x)$summ_foot
  # })
  # footnotes <- c(unlist(x), footnotes)

  # Define column header
  colheader <- c(
    paste0(" | Event Count and Rate (Events/100 person-", time_unit, "){^a}"),
    paste0(" | ", paste(group, collapse = " | "))
  )

  # Relative width
  if (is.null(col_rel_width)) {
    rel_width_body <- c(3, rep(2, n_group), 1)
  } else {
    rel_width_body <- col_rel_width
  }

  rel_width_head <- rel_width_body[1:(length(rel_width_body) - 1)]
  rel_width_head <- list(
    c(3, sum(rep(2, n_group))),
    rel_width_head
  )

  # column boarder
  border_top_head <- c("", rep("single", n_group))
  border_top_body <- c(rep("", 1 + n_group), "single")
  border_left_head <- c("single", rep("single", n_group))
  border_left_body <- c(border_left_head, "single")

  text_format <- c(rep("", 1 + n_group), "b")

  # using order number to customize row format
  text_justification <- c("l", rep("c", n_group), "l")
  text_indent <- matrix(0, nrow = n_row, ncol = n_col)
  text_indent[, 1] <- ifelse(FALSE, 0, 100)
  text_indent[1:2, 1] <- 0

  # Use r2rtf
  outdata$rtf <- tbl |>
    r2rtf::rtf_page(orientation = orientation) |>
    r2rtf::rtf_title(title) |>
    r2rtf::rtf_colheader(
      colheader = colheader[1],
      col_rel_width = rel_width_head[[1]],
      text_font_size = text_font_size
    ) |>
    r2rtf::rtf_colheader(
      colheader = colheader[2],
      border_top = border_top_head,
      border_left = border_left_head,
      col_rel_width = rel_width_head[[2]],
      text_font_size = text_font_size
    ) |>
    r2rtf::rtf_body(
      page_by = "row_label",
      col_rel_width = rel_width_body,
      border_left = border_left_body,
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
