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
#' @return An `lt_tbl` object for an AE summary table.
#'
#' @export
#'
#' @examples
#' library(lt)
#' #' # Define metadata
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
#'   gt_ae_summary(
#'     analysis = "ae_summary",
#'     source = "Source:  [CDISCpilot: adam-adsl; adae]"
#'   )
gt_ae_summary <- function(outdata,
                          source = NULL,
                          analysis,
                          title = c("analysis", "observation", "population"),
                          footnotes = NULL) {
  tbl <- outdata$tbl
  group <- outdata$group
  num_groups <- length(group)

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
  }

  x <- lapply(parameters, function(x) {
    collect_adam_mapping(outdata$meta, x)$summ_foot
  })
  footnotes <- c(unlist(x), footnotes)

  name_col <- colnames(tbl)[1] # first column, e.g. "name"
  cols_label <- c(
    stats::setNames("", name_col),
    stats::setNames(rep("n", num_groups), paste0("n_", seq_len(num_groups))),
    stats::setNames(rep("(%)", num_groups), paste0("prop_", seq_len(num_groups)))
  )

  tbl[[name_col]] <- convert_caret_sup(tbl[[name_col]])

  out <- lt::lt(tbl, auto_format = FALSE) |>
    lt::lt_sub(missing = "") |>
    lt::lt_html(name_col) |>
    lt::lt_label(cols_label) |>
    lt::lt_header(title = I(paste(title, collapse = "<br>")))

  # One spanner per treatment group over its n/(%) column pair.
  for (i in seq_along(group)) {
    out <- lt::lt_spanner(out,
      label = group[i],
      columns = c(paste0("n_", i), paste0("prop_", i))
    )
  }

  # Footnotes and source render as table notes at the bottom.
  for (note in c(footnotes, source)) {
    if (!is.null(note) && nzchar(note)) {
      out <- lt::lt_note(out, I(convert_caret_sup(note)))
    }
  }

  out
}
