#    Copyright (c) 2022 Merck Sharp & Dohme Corp. a subsidiary of Merck & Co., Inc., Kenilworth, NJ, USA.
#
#    This file is part of the metalite.ae program.
#
#    metalite.ae is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Specific Adverse Events Table
#'
#' @param outdata a outdata list created from `prepare_ae_specific`
#' @param medra_version a character value of the MedDRA Version for this dataset
#' @param source a character value of the data source
#' @inheritParams r2rtf::rtf_page
#' @inheritParams r2rtf::rtf_body
#' @param footnotes a character vector of table footnotes
#' @param path_outdata a character string of the outdata path.
#' @param path_outtable a character string of the outtable path.
#'
#' @export
#'
#' @examples
#' library(r2rtf)
#' library(metalite)
#' meta <- meta_ae_dummy()
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
#'     medra_version = "24.0",
#'     path_outdata = tempfile(fileext = ".Rdata"),
#'     path_outtable = tempfile(fileext = ".rtf")
#'   )
tlf_ae_specific <- function(outdata,
                            medra_version,
                            source,
                            col_rel_width = NULL,
                            text_font_size = 9,
                            orientation = "portrait",
                            footnotes = NULL,
                            path_outdata = NULL,
                            path_outtable = NULL) {
  
  if(is.null(footnotes)){
    footnotes <- c(
      "Every participant is counted a single time for each applicable row and column.",
      paste(
        "A system organ class or specific adverse event appears on this report only if",
        "its incidence in one or more of the columns meets the incidence",
        "criterion in the report title, after rounding."
      ),
      "Adverse event terms are from MedDRA Version {medra_version}."
    )
  }
  tbl <- outdata$tbl
  group <- outdata$group
  reference_group <- outdata$reference_group
  n_group <- length(outdata$group)
  n_row <- nrow(tbl)
  n_col <- ncol(tbl)

  # Define title
  title <- collect_title(outdata$meta, outdata$population, outdata$observation, outdata$parameter, analysis = "ae_specific")
  footnotes <- vapply(footnotes, glue::glue_data,
    .x = list(medra_version = medra_version), FUN.VALUE = character(1)
  )
  names(footnotes) <- NULL

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

  if (length(outdata$components) == 2) {
    text_format <- ifelse(outdata$order %% 1000 == 0, "b", "")
  } else {
    text_format <- ""
  }
  text_format <- matrix(text_format, nrow = n_row, ncol = n_col)

  text_indent <- matrix(0, nrow = n_row, ncol = n_col)
  text_indent[, 1] <- ifelse(outdata$order %% 1000 == 0 | outdata$order == 1, 0, 100)

  # Using r2rtf

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


  # prepare output
  rtf_output(outdata, path_outdata, path_outtable)
}
