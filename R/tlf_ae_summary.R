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
#' @return To be added.
#'
#' @export
#'
#' @examples
#' # To be added
tlf_ae_summary <- function(outdata,
                           source,
                           col_rel_width = NULL,
                           text_font_size = 9,
                           orientation = "portrait",
                           footnotes = NULL,
                           path_outdata = NULL,
                           path_outtable = NULL) {
  tbl <- outdata$tbl
  group <- outdata$group
  reference_group <- outdata$reference_group
  n_group <- length(outdata$group)
  n_row <- nrow(tbl)
  n_col <- ncol(tbl)

  parameters <- unlist(strsplit(outdata$parameter, ";"))

  # Title
  title <- collect_title(outdata$meta, outdata$population, outdata$observation, parameters[1], analysis = "ae_summary")

  # Footnotes
  x <- lapply(parameters, function(x) {
    collect_adam_mapping(outdata$meta, x)$summ_foot
  })
  footnotes <- c(unlist(x), footnotes)

  # Define column header
  colheader_n <- c(
    paste0(" | ", paste(group, collapse = " | ")),
    paste0(" | ", paste(rep("n | (%)", n_group), collapse = " | "))
  )

  # TODO: add logic for CI and p-value with multipel groups following WMA mock up table.
  # colheader_ci <- c(paste("Difference in % vs", group[reference_group]),
  # "Estimate (95% CI)")

  # colheader_p <- c("", "p-value")
  # colheader <- paste(colheader_n, colheader_ci, colheader_p, sep = " | ")

  colheader <- colheader_n

  # Relative width
  if (is.null(col_rel_width)) {
    rel_width <- c(3, rep(1, 2 * n_group))
  } else {
    rel_width <- col_rel_width
  }

  n_col <- length(rel_width)

  rel_width1 <- c(
    rel_width[1],
    tapply(rel_width[2:(n_group * 2 + 1)], c(rep(1:n_group, each = 2)), sum),
    rel_width[-(1:(n_group * 2 + 1))]
  )

  # Column boarder
  border_top <- c("", rep("single", n_col - 1))
  border_left <- c("single", rep(c("single", ""), n_group), rep("single", n_col - n_group * 2 - 1))

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
