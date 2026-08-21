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

#' Prepare datasets for AE summary
#'
#' @inheritParams prepare_ae_specific
#' @param ... Additional arguments passed to [prepare_ae_specific()].
#'
#' @return An `outdata` object containing analysis datasets needed for AE
#'   summary. Key values include:
#'
#'   - `group`: Treatment groups used to index the statistic columns.
#'   - `order`: Numeric values defining the row order.
#'   - `name`: Descriptive row labels corresponding to `order`.
#'   - `n_pop`: Number of participants in the population.
#'   - `n`: Number of participants with an adverse event.
#'   - `prop`: Proportion of participants with an adverse event.
#'   - `diff`: Risk difference compared with the `reference_group`.
#'
#' @export
#'
#' @examples
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
#' prepare_ae_summary(
#'   meta,
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "any;rel;ser"
#' )
prepare_ae_summary <- function(meta,
                               population,
                               observation,
                               parameter,
                               ...) {
  parameters <- unlist(strsplit(parameter, ";"))

  res <- lapply(parameters, function(x) {
    message(x)
    prepare_ae_specific(meta,
      population = population, observation = observation, parameter = x,
      components = NULL, ...
    )
  })

  n_pop <- res[[1]]$n_pop
  tbl_num <- do.call(rbind, lapply(res, function(x) x$n[x$order == 100, , drop = FALSE]))

  pop_prop <- res[[1]]$prop[1, , drop = FALSE]
  tbl_prop <- do.call(rbind, lapply(res, function(x) x$prop[x$order == 100, , drop = FALSE]))

  pop_diff <- res[[1]]$diff[1, , drop = FALSE]
  tbl_diff <- do.call(rbind, lapply(res, function(x) x$diff[x$order == 100, , drop = FALSE]))

  pop_name <- res[[1]]$name[1]
  name <- unlist(lapply(parameters, function(x) collect_adam_mapping(meta, x)$summ_row))

  # Extract the data for 'with no ae' row only when parameter 'any' is provided.
  if ("any" %in% parameters) {
    names(res) <- parameters

    # Extract the values for 'with no ae' row.
    noevnt_num <- res$any$n[3, , drop = FALSE]
    noevnt_prop <- res$any$prop[3, , drop = FALSE]
    noevnt_diff <- res$any$diff[3, , drop = FALSE]
    noevnt_name <- res$any$name[3]

    # Combine records with original other parameters and sort df
    rbind1 <- function(df1, df2) {
      df1 <- rbind(df1, df2)
      df1 <- df1[order(as.numeric(row.names(df1))), , drop = FALSE]
      df1
    }

    tbl_num <- rbind1(tbl_num, noevnt_num)
    tbl_prop <- rbind1(tbl_prop, noevnt_prop)
    tbl_diff <- rbind1(tbl_diff, noevnt_diff)
    # tbl_ci <- rbind1(tbl_ci, noevnt_ci)
    # tbl_p <- rbind1(tbl_p, noevnt_p)
    name <- append(name, noevnt_name, 1)

    names(res) <- NULL
  }

  metalite::outdata(meta, population, observation, parameter,
    n = rbind(n_pop, tbl_num),
    order = c(1, seq_len(nrow(tbl_num)) * 100),
    group = res[[1]]$group,
    reference_group = res[[1]]$reference_group,
    prop = rbind(pop_prop, tbl_prop),
    diff = rbind(pop_diff, tbl_diff),
    n_pop = n_pop,
    name = c(pop_name, name),
    prepare_call = match.call()
  )
}
