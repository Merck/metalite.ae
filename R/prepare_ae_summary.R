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
#' @return A list of analysis datasets needed for AE summary.
#'
#' @export
#'
#' @examples
#' meta <- meta_ae_example()
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
  tbl_num <- do.call(rbind, lapply(res, function(x) x$n[x$order == 100, ]))

  pop_prop <- res[[1]]$prop[1, ]
  tbl_prop <- do.call(rbind, lapply(res, function(x) x$prop[x$order == 100, ]))

  pop_diff <- res[[1]]$diff[1, ]
  tbl_diff <- do.call(rbind, lapply(res, function(x) x$diff[x$order == 100, ]))

  pop_ci <- res[[1]]$ci[1, ]
  tbl_ci <- do.call(rbind, lapply(res, function(x) x$ci[x$order == 100, ]))

  pop_p <- res[[1]]$p[1, ]
  tbl_p <- do.call(rbind, lapply(res, function(x) x$p[x$order == 100, ]))

  pop_name <- res[[1]]$name[1]
  name <- unlist(lapply(parameters, function(x) collect_adam_mapping(meta, x)$summ_row))

  # Extract the data for 'with no ae' row only when parameter 'any' is provided.
  if ("any" %in% parameters) {
    names(res) <- parameters

    # Extract the values for 'with no ae' row.
    noevnt_num <- res$any$n[3, ]
    noevnt_prop <- res$any$prop[3, ]
    noevnt_diff <- res$any$diff[3, ]
    noevnt_ci <- res$any$ci[3, ]
    noevnt_p <- res$any$p[3, ]
    noevnt_name <- res$any$name[3]

    # Combine records with original other parameters and sort df
    rbind1 <- function(df1, df2) {
      df1 <- rbind(df1, df2)
      df1 <- df1[order(as.numeric(row.names(df1))), ]
      df1
    }

    tbl_num <- rbind1(tbl_num, noevnt_num)
    tbl_prop <- rbind1(tbl_prop, noevnt_prop)
    tbl_diff <- rbind(tbl_diff, noevnt_diff)
    tbl_ci <- rbind(tbl_ci, noevnt_ci)
    tbl_p <- rbind(tbl_p, noevnt_p)
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

#' Format AE summary analysis
#'
#' @inheritParams format_ae_specific
#'
#' @return A list of analysis raw datasets.
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
#' tbl <- outdata |>
#'   format_ae_summary()
#' head(tbl$tbl)
format_ae_summary <- function(outdata,
                              display = c("n", "prop", "total"),
                              hide_soc_stats = FALSE,
                              digits_prop = 1,
                              digits_ci = 1,
                              digits_p = 3,
                              digits_dur = c(1, 1),
                              digits_events = c(1, 1),
                              filter_method = c("percent", "count"),
                              filter_criteria = 0,
                              sort_order = c("alphabetical", "count_des", "count_asc"),
                              sort_column = NULL,
                              mock = FALSE) {
  if (length(outdata$name) != nrow(outdata$n)) {
    parameters <- unlist(strsplit(outdata$parameter, ";"))
    summary_names <- vapply(parameters, function(parameter) {
      mapping <- collect_adam_mapping(outdata$meta, parameter)

      if (is.null(mapping$summ_row)) mapping$label else mapping$summ_row
    }, character(1))

    if ("any" %in% parameters) {
      summary_names <- append(
        summary_names,
        "with no adverse events",
        after = 1
      )
    }

    outdata$name <- c(outdata$name[1], summary_names)
  }

  format_ae_specific(
    outdata = outdata,
    display = display,
    hide_soc_stats = hide_soc_stats,
    digits_prop = digits_prop,
    digits_ci = digits_ci,
    digits_p = digits_p,
    digits_dur = digits_dur,
    digits_events = digits_events,
    filter_method = filter_method,
    filter_criteria = filter_criteria,
    sort_order = sort_order,
    sort_column = sort_column,
    mock = mock
  )
}
