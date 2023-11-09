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

#' Add exposure-adjusted rate information for AE summary analysis
#'
#' @param outdata  An `outdata` object created by [prepare_ae_summary()].
#' @param duration_var A character value of duration variable name.
#'   By default, `"TRTDUR"` is used.
#' @param adj_unit A character value of exposure adjusted unit.
#'   It could be select from `"year"`, `"month"`, `"week"`, and `"day"`.
#'
#' @return A list of analysis raw datasets.
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
#' ) |>
#'   extend_ae_summary_eaer()
extend_ae_summary_eaer <- function(outdata,
                                   duration_var = "TRTDUR",
                                   adj_unit = c("year", "month", "week", "day")) {
  time_unit <- list("year" = 365.24, "month" = 30.4367, "week" = 7, "day" = 1)
  adj_unit <- match.arg(adj_unit)
  exp_factor <- 100 * time_unit[[adj_unit]]

  # prep
  pop_var <- collect_adam_mapping(outdata$meta, outdata$population)$var
  pop <- collect_population_record(outdata$meta, outdata$population, var = c(pop_var, duration_var))
  pop_group <- collect_adam_mapping(outdata$meta, outdata$population)$group

  # den: Total exposure in person-year/month/week/day
  total_exposure <- tapply(pop[[duration_var]], pop[[pop_group]], FUN = sum)


  parameters <- unlist(strsplit(outdata$parameter, ";"))

  res <- lapply(parameters, function(x) {
    message(x)
    den <- total_exposure
    num <- f_nae(x, outdata$meta, outdata$population)
    ans <- num * exp_factor / den
  })

  adj_rate_table <- do.call(rbind, res)
  outdata$total_exp <- total_exposure
  outdata$eaer <- adj_rate_table

  outdata
}


f_nae <- function(x, meta, observation) {
  # prep
  obs_group <- collect_adam_mapping(meta, observation)$group

  # start computation
  if (x == "any") {
    data <- meta$data_observation
    num <- sapply(split(data, data[[obs_group]]), function(x) length(x[[obs_group]]))
  } else {
    data <- meta$data_observation
    expr <- collect_adam_mapping(meta, x)$subset
    data <- data %>% subset(eval(expr))
    num <- sapply(split(data, data[[obs_group]]), function(x) length(x[[obs_group]]))
  }
  return(num)
}
