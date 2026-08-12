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

#' Specific adverse events table in gt format
#'
#' @inheritParams tlf_ae_specific
#'
#' @return A `gt_tbl` object for an AE specific table.
#'
#' @export
#'
#' @examples
#' library(gt)
#'
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
#'   analysis = "ae_specific",
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "rel"
#' )
#'
#' meta <- metalite::meta_adam(observation = adae, population = adsl) |>
#'   metalite::define_plan(analysis_plan) |>
#'   metalite::define_population(
#'     name = "apat",
#'     var = c("USUBJID", "SAFFL", "TRT01A", "SITEID", "SEX", "RACE", "AGE"),
#'     group = "TRT01A",
#'     subset = SAFFL == "Y",
#'     label = "All Participants as Treated"
#'   ) |>
#'   metalite::define_observation(
#'     name = "wk12",
#'     var = c(
#'       "USUBJID", "SAFFL", "TRTA", "SEX", "AEDECOD", "AEBODSYS",
#'       "AEREL", "AESER", "AEOUT", "AEACN", "AESDTH", "ASTDT", "AENDT"
#'     ),
#'     group = "TRTA",
#'     subset = SAFFL == "Y",
#'     label = "Weeks 0 to 12"
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
#'   metalite::define_analysis(
#'     name = "ae_specific",
#'     title = "Participants With Drug-Related Adverse Events"
#'   ) |>
#'   metalite::meta_build()
#'
#' meta |>
#'   prepare_ae_specific(
#'     population = "apat",
#'     observation = "wk12",
#'     parameter = "rel"
#'   ) |>
#'   format_ae_specific() |>
#'   gt_ae_specific(
#'     meddra_version = "24.0",
#'     source = "Source:  [CDISCpilot: adam-adsl; adae]",
#'     analysis = "ae_specific"
#'   )
gt_ae_specific <- function(outdata,
                           meddra_version,
                           source,
                           analysis,
                           footnotes = NULL,
                           title = c("analysis", "observation", "population")) {
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
  n_group <- length(group)

  analysis_name <- names(outdata$meta$analysis)
  if (!(analysis %in% analysis_name)) {
    stop(
      "Please provide a valid analysis that matches with what being defined in 'outdata$meta$analysis'",
      call. = FALSE
    )
  }

  if ("analysis" %in% title || "observation" %in% title || "population" %in% title) {
    title_key <- title
    title_text <- collect_title(outdata$meta,
      outdata$population,
      outdata$observation,
      outdata$parameter,
      analysis = analysis,
      title_order = title
    )

    filter_criteria_text <- paste0(
      ifelse(outdata$filter_criteria > 0, "\\geq ", "> "),
      outdata$filter_criteria
    )
    filter_method_text <- ifelse(
      outdata$filter_method == "percent",
      "%",
      "Participants with an Adverse Event"
    )
    filter_text <- paste("(Incidence", filter_criteria_text, filter_method_text)
    filter_text <- ifelse(n_group > 1,
      paste(filter_text, "in One or More Treatment Groups)"),
      paste(filter_text, ")")
    )

    if ("analysis" %in% title_key) {
      title <- unlist(lapply(seq_along(title_key), function(index) {
        if (title_key[[index]] == "analysis") {
          c(title_text[[index]], filter_text)
        } else {
          title_text[[index]]
        }
      }))
    } else {
      title <- title_text
    }
  }

  footnotes <- vapply(footnotes, glue::glue_data,
    .x = list(meddra_version = meddra_version), FUN.VALUE = character(1)
  )
  names(footnotes) <- NULL

  convert_caret_sup <- function(x) {
    if (is.null(x)) {
      return(x)
    }

    x_char <- as.character(x)
    na_index <- is.na(x_char)
    x_char <- gsub("<", "&lt;", x_char, fixed = TRUE)
    x_char <- gsub(">", "&gt;", x_char, fixed = TRUE)
    x_char <- gsub("\\{\\^([^}]+)\\}", "<sup>\\1</sup>", x_char, perl = TRUE)
    x_char[na_index] <- NA_character_
    x_char
  }

  name_col <- names(tbl)[1]
  tbl[[name_col]] <- convert_caret_sup(tbl[[name_col]])
  combined_title_md <- paste(title, collapse = "  \n")

  metric_labels <- c(
    n = "n",
    prop = "(%)",
    dur = "Mean Duration (SE)",
    eventsavg = "Mean Events per Participant (SE)",
    eventscount = "Number of Events",
    diff = "Estimate",
    ci = paste0("(", outdata$ci_level * 100, "% CI)"),
    p = "p-value"
  )
  metric <- sub("_[0-9]+$", "", names(tbl)[-1])
  cols_label_vec <- c(
    stats::setNames("", name_col),
    stats::setNames(unname(metric_labels[metric]), names(tbl)[-1])
  )

  within_spanners <- lapply(seq_along(group), function(index) {
    columns <- grep(paste0("_", index, "$"), names(tbl), value = TRUE)
    columns <- columns[!grepl("^(diff|ci|p)_", columns)]
    list(label = group[index], columns = columns)
  })

  comparison_columns <- grep("^(diff|ci|p)_[0-9]+$", names(tbl), value = TRUE)
  comparison_indices <- unique(sub("^.*_", "", comparison_columns))
  comparison_spanners <- lapply(comparison_indices, function(index) {
    list(
      label = paste(
        "Difference in %", group[as.integer(index)], "vs.",
        group[outdata$reference_group]
      ),
      columns = comparison_columns[grepl(paste0("_", index, "$"), comparison_columns)]
    )
  })

  spanners <- c(within_spanners, comparison_spanners)
  spanners <- spanners[lengths(lapply(spanners, `[[`, "columns")) > 0]

  gt_tbl <- tbl |>
    gt::gt() |>
    gt::sub_missing(columns = seq_len(ncol(tbl)), missing_text = "") |>
    gt::fmt_markdown(columns = 1) |>
    gt::tab_header(title = gt::md(combined_title_md)) |>
    (\(gt_tbl) Reduce(function(result, spanner) {
      gt::tab_spanner(result,
        label = spanner$label,
        columns = spanner$columns
      )
    }, spanners, init = gt_tbl))() |>
    gt::cols_label(!!!cols_label_vec)

  soc_rows <- which(outdata$order %% 1000 == 0)
  if (length(outdata$components) == 2 && length(soc_rows) > 0) {
    gt_tbl <- gt_tbl |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(rows = soc_rows)
      )
  }

  indented_rows <- which(outdata$order %% 1000 != 0 & outdata$order != 1)
  if (length(indented_rows) > 0) {
    gt_tbl <- gt_tbl |>
      gt::tab_style(
        style = gt::cell_text(indent = gt::px(10)),
        locations = gt::cells_body(columns = 1, rows = indented_rows)
      )
  }

  if (length(footnotes) > 0) {
    gt_tbl <- gt_tbl |>
      gt::tab_source_note(gt::md(convert_caret_sup(footnotes)))
  }
  if (!is.null(source) && nzchar(source)) {
    gt_tbl <- gt_tbl |>
      gt::tab_source_note(gt::md(convert_caret_sup(source)))
  }

  gt_tbl
}