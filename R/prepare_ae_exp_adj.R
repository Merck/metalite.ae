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

#' Prepare datasets for AE specific analysis
#'
#' @param meta A metadata object created by metalite.
#' @param population A character value of population term name.
#'   The term name is used as key to link information.
#' @param observation A character value of observation term name.
#'   The term name is used as key to link information.
#' @param parameter A character value of parameter term name.
#'   The term name is used as key to link information.
#' @param duration_var A character value of duration variable name.
#    By default "TRTDUR" is used.
#' @param adj_unit A character value of exposure adjusted unit.
#    It could be select from "year", "month", "week" and "day".
#' @return A list of analysis raw datasets.
#'
#' @export
#'
#' @examples
#' meta <- meta_ae_example()
#' prepare_ae_exp_adj(meta)
prepare_ae_exp_adj <- function(meta,
                               population = "apat",
                               observation = "wk12",
                               parameter = "any;rel;ser",
                               duration_var = "TRTDUR",
                               adj_unit = c("year", "month", "week", "day")) {
  time_unit <- list("year" = 365.24, "month" = 30.4367, "week" = 7, "day" = 1)
  adj_unit <- match.arg(adj_unit)
  exp_factor <- 100 * time_unit[[adj_unit]]

  # obtain variables
  pop_var <- collect_adam_mapping(meta, population)$var
  obs_var <- collect_adam_mapping(meta, observation)$var
  par_var <- collect_adam_mapping(meta, parameter)$var
  par_soc <- collect_adam_mapping(meta, parameter)$soc

  pop_group <- collect_adam_mapping(meta, population)$group
  obs_group <- collect_adam_mapping(meta, observation)$group

  pop_id <- collect_adam_mapping(meta, population)$id
  obs_id <- collect_adam_mapping(meta, observation)$id

  # obtain data
  pop <- collect_population_record(meta, population, var = c(pop_var, duration_var))
  obs <- collect_observation_record(meta, population, observation, parameter, var = unique(c(obs_var, par_var, par_soc)))

  # number of Subjects exposed
  n_exposed <- metalite::n_subject(id = pop[[pop_id]], group = pop[[pop_group]])

  # exposure adjust evnt rate = total number of event * exp_factor / total_exposure_days
  parameters <- unlist(strsplit(parameter, ";"))

  # total exposure in person-year/month/week/day
  total_exposure <- tapply(pop$TRTDUR, pop[[pop_group]], FUN = sum)
  names(total_exposure) <- c("group", "tol_exp")

  res <- lapply(parameters, function(x) {
    data <- meta$data_observation
    if (x == "any") {
      num <- sapply(split(data, data[[obs_group]]), function(x) length(x[[obs_group]]))
      ans <- num * exp_factor / total_exposure[["tol_exp"]]
    } else {
      # count the number of events either serious or drug-related or ... depending on the parameter
      trt_grps <- levels(data[[obs_group]])
      num_grps <- length(trt_grps)
      num <- rep(NA, num_grps)
      for (j in 1:num_grps) {
        expr <- collect_adam_mapping(meta, x)$subset
        data_j <- data |> subset(data[[obs_group]] == trt_grps[j])
        temp_j <- eval(expr = substitute(expr), envir = substitute(data_j))
        num[j] <- sum(temp_j)
      }
      ans <- num * exp_factor / total_exposure[["tol_exp"]]
    }

    return(ans)
  })

  metalite::outdata(meta, population, observation, parameter,
    n = n_exposed, order = NULL, group = pop_group,
    reference_group = NULL,
    total_exposure = total_exposure,
    adj_rate = res,
    group_label = unique(pop[[pop_group]])
  )
}
