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
#' @param analysis Analysis name from `meta`.
#' @param population A character value of population term name.
#'   The term name is used as key to link information.
#' @param observation A character value of observation term name.
#'   The term name is used as key to link information.
#' @param parameter A character value of parameter term name.
#'   The term name is used as key to link information.
#'
#' @return To be added.
#'
#' @import metalite
#'
#' @export
#'
#' @examples
#' meta <- meta_ae_listing_example()
#' lapply(prepare_ae_listing(meta, "ae_listing", "apat", "wk12", "ser"), head, 10)
prepare_ae_listing <- function(meta,
                               analysis,
                               population,
                               observation,
                               parameter) {
  mapping <- collect_adam_mapping(meta, analysis)
  var_name <- eval(mapping$var_name)
  subline <- eval(mapping$subline)
  subline_by <- eval(mapping$subline_by)
  group_by <- eval(mapping$group_by)
  page_by <- eval(mapping$page_by)

  var_names <- c(var_name, subline, subline_by, group_by, page_by)

  res <- collect_observation_record(meta, population, observation, parameter,
    var = var_names
  )

  res <- res[names(res) %in% var_names]

  # Sort res data frame by order of var_names
  res <- res[, unique(var_names)]

  # Extract label from data frame as column name of listing
  col_name <- get_label(res)

  # Return value
  outdata(meta, population, observation, parameter,
    n = NULL, order = NULL, group = NULL, reference_group = NULL,
    col_name = col_name, tbl = res
  )
}
