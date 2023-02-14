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
#' @param outdata a outdata list created from `prepare_ae_listing`
#' @param footnotes a character vector of table footnotes
#' @param source a character value of the data source
#' @inheritParams r2rtf::rtf_page
#' @inheritParams r2rtf::rtf_body
#' @param path_outdata a character string of the outdata path
#' @param path_outtable a character string of the outtable path
#'
#' @return To be added.
#'
#' @export
#'
#' @examples
#' library(r2rtf)
#' library(metalite)
#'
#' meta <- meta_ae_listing_example()
#' prepare_ae_listing(meta, "ae_listing", "apat", "wk12", "ser") |>
#'   tlf_ae_listing(
#'     footnotes = "footnote1",
#'     source = "Source:  [CDISCpilot: adam-adsl; adae]",
#'     path_outdata = tempfile(fileext = ".Rdata"),
#'     path_outtable = tempfile(fileext = ".rtf")
#'   )
tlf_ae_listing <- function(outdata,
                           footnotes = NULL,
                           source = NULL,
                           col_rel_width = NULL,
                           text_font_size = 9,
                           orientation = "landscape",
                           path_outdata = NULL,
                           path_outtable = NULL) {
  res <- outdata$tbl

  mapping <- collect_adam_mapping(outdata$meta, "ae_listing")
  var_name <- eval(mapping$var_name)
  subline <- eval(mapping$subline)
  subline_by <- eval(mapping$subline_by)
  group_by <- eval(mapping$group_by)
  page_by <- eval(mapping$page_by)
  ae_var <- collect_adam_mapping(outdata$meta, outdata$parameter)$var
  col_name <- outdata$col_name

  # Define title
  title <- collect_title(outdata$meta, outdata$population, outdata$observation, outdata$parameter, analysis = "ae_listing")

  res <- as.data.frame(res)
  res <- res[do.call(what = order, args = res[, c(page_by, group_by, ae_var)]), ]

  # Relative width
  n_col <- ncol(res)
  if (is.null(col_rel_width)) {
    rel_width <- rep(1, n_col)
  } else {
    rel_width <- col_rel_width
  }

  rel_width1 <- c()
  for (i in 1:n_col) {
    if (names(res)[i] %in% var_name) {
      rel_width1 <- c(rel_width1, col_rel_width[i])
    }
  }

  # Text justification
  text_justification <- c()
  for (i in 1:n_col) {
    if (i == 1) {
      text_justification <- c(text_justification, "l")
    } else if (names(res)[i] %in% var_name) {
      text_justification <- c(text_justification, "c")
    } else if (names(res)[i] %in% subline_by) {
      text_justification <- c(text_justification, "l")
    } else {
      text_justification <- c(text_justification, "l")
    }
  }

  # Text format
  text_format <- c()
  for (i in 1:n_col) {
    if (names(res)[i] %in% var_name) {
      text_format <- c(text_format, "")
    } else if (names(res)[i] %in% page_by[2:length(page_by)]) {
      text_format <- c(text_format, "")
    } else {
      text_format <- c(text_format, "b")
    }
  }

  # Define column header
  colheader_display <- col_name[!names(res) %in% c(page_by, subline_by)]

  colheader <- paste(colheader_display, collapse = " | ")
  colheader <- gsub("_", " ", colheader)

  # Use r2rtf
  outdata$rtf <- res |>
    r2rtf::rtf_page(orientation = orientation) |>
    r2rtf::rtf_title(title) |>
    r2rtf::rtf_colheader(colheader,
      col_rel_width = rel_width1,
      text_font_size = text_font_size
    ) |>
    r2rtf::rtf_body(
      col_rel_width = rel_width,
      text_justification = text_justification,
      text_format = text_format,
      # border_top = c(rep("", 6), "single","single"),
      # border_bottom = c(rep("", 6), "single","single"),
      # border_left = c(rep(c("single"), 11) ),
      page_by = page_by,
      group_by = group_by,
      subline_by = subline_by,
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
