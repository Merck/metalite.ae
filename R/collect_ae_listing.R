#    Copyright (c) 2022 Merck & Co., Inc., Rahway, NJ, USA and its affiliates. All rights reserved.
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

#' Add inference information for AE listing analysis
#'
#' @param outdata a `outdata` object created by `prepare_ae_specific`
#' @param display a vector with name of variable used to display on AE listing.
#'
#' @import metalite
#' @examples
#' library(metalite)
#' meta <- meta_ae_dummy()
#'
#' lapply(prepare_ae_specific(meta, "apat", "wk12", "rel") |>
#' collect_ae_listing(), head, 10)
#'
#' @export

collect_ae_listing <- function(outdata,
                               display = c("SEX", "RACE", "AGE", "ASTDY","AESEV","AESER",
                                           "AEREL","AEACN","AEOUT", "SITEID","ADURN", "ADURU")) {

  obs_group <- collect_adam_mapping(outdata$meta, outdata$observation)$group
  obs_id <- collect_adam_mapping(outdata$meta, outdata$observation)$id
  par_var <- collect_adam_mapping(outdata$meta, outdata$parameter)$var

  obs <- collect_observation_record(outdata$meta, outdata$population, outdata$observation, outdata$parameter,
                                    var = c(par_var, obs_id, obs_group, display)
  )

  outdata$ae_listing <- obs[,c(par_var, obs_id, obs_group, display)]
  outdata
}
