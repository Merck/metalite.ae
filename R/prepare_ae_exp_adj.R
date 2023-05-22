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
#'
#' @return A list of analysis raw datasets.
#'
#' @export
#'
#' @examples
prepare_ae_exp_adj <- function(meta,
                               population = "apat",
                               observation = "wk12",
                               parameter = "any;rel;ser",
                               adj_unit = c("year", "month", "week", "day"),
                               ...) {
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
  pop <- collect_population_record(meta, population, var = pop_var)
  obs <- collect_observation_record(meta, population, observation, parameter, var = unique(c(obs_var, par_var, par_soc)))

  # number of Subjects exposed
  n_exposed <- metalite:::n_subject(id = pop[[pop_id]], group = pop[[pop_group]])

  # exposure adjust evnt rate = total number of event * exp_factor / total_exposure_days
  parameters <- unlist(strsplit(parameter, ";"))

  # total exposure in person-year/month/week/day
  total_exposure <- aggregate(pop$TRTDUR , by = list(pop[[pop_group]]), FUN = sum)
  names(total_exposure) <- c("group", "tol_exp")

  res <- lapply(parameters, function(x) {
    if(x == "any"){
      ans <- nrow(meta$data_observation) * exp_factor / total_exposure$tol_exp
    } else {
      # count the number of events either serious or drug-related or ... depending on the parameter
      temp <- rlang::eval_tidy(
        expr = collect_adam_mapping(meta, x)$subset,
        data = meta$data_observation
      )
      ans <- sum(temp) * exp_factor / total_exposure$tol_exp
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