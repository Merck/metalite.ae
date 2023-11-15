# Copyright (c) 2023 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
# All rights reserved.
#
# This file is part of the metalite.ae program.
#
# metalite.ae is a free software: you can redistribute it and/or modify
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

#' Prepare datasets for AE specific subgroup analysis
#'
#' @inheritParams prepare_ae_specific
#' @param subgroup_var A character value of subgroup variable name in
#'   observation data saved in `meta$data_observation`.
#' @param subgroup_header A character vector for column header hierarchy.
#'   The first element will be the first level header and the second element
#'   will be second level header.
#' @param display_subgroup_total Logical. Display total column for
#'   subgroup analysis or not.
#'
#' @return A list of analysis datasets needed for AE specific subgroup analysis.
#'
#' @export
#'
#' @examples
#' meta <- meta_ae_example()
#' prepare_ae_specific_subgroup(meta, "apat", "wk12", "rel", subgroup_var = "SEX")$data
prepare_ae_specific_subgroup <- function(
    meta,
    population,
    observation,
    parameter,
    subgroup_var,
    subgroup_header = c(meta$population[[population]]$group, subgroup_var),
    components = c("soc", "par"),
    display_subgroup_total = TRUE) {
  meta_original <- meta

  meta$data_population[[subgroup_var]] <- factor(
    as.character(meta$data_population[[subgroup_var]]),
    levels = sort(unique(meta$data_population[[subgroup_var]]))
  )
  meta$data_observation[[subgroup_var]] <- factor(
    as.character(meta$data_observation[[subgroup_var]]),
    levels = sort(unique(meta$data_observation[[subgroup_var]]))
  )

  meta$observation[[observation]]$group <- subgroup_header[1]
  meta$population[[population]]$group <- subgroup_header[1]

  meta$data_observation <- collect_observation_record(
    meta,
    population = population,
    observation = observation,
    parameter = parameter,
    var = names(meta$data_observation)
  )

  # Obtain variables
  par_var <- collect_adam_mapping(meta, parameter)$var
  par_soc <- collect_adam_mapping(meta, parameter)$soc

  # Convert variable to factor
  meta$data_observation[[par_var]] <- factor(
    as.character(meta$data_observation[[par_var]]),
    levels = sort(unique(meta$data_observation[[par_var]]))
  )

  meta$data_observation[[par_soc]] <- factor(
    as.character(meta$data_observation[[par_soc]]),
    levels = sort(unique(meta$data_observation[[par_soc]]))
  )

  meta_subgroup <- metalite::meta_split(meta, subgroup_header[2])

  outdata_all <- prepare_ae_specific(
    meta,
    population = population,
    observation = observation,
    parameter = parameter,
    components = components
  )

  # Currently, Total column analysis is supported for trt_within_sub.
  # Need programming for Total column for sub_within_trt.
  outdata_subgroup <- lapply(
    meta_subgroup,
    prepare_ae_specific,
    population = population,
    observation = observation,
    parameter = parameter,
    components = components
  )

  # Current output for subgroup analysis is supported for trt_within_sub.
  # output of subgroup analysis needs restructuring for sub_within_trt.
  out_all <- outdata_subgroup
  out_all$Total <- outdata_all

  group <- outdata_subgroup[[1]]$group
  group <- group[!group %in% "Total"]

  outdata <- list(
    components = outdata_subgroup[[1]]$components,
    group = group,
    subgroup = tools::toTitleCase(tolower(names(outdata_subgroup))),
    display_subgroup_total = display_subgroup_total,
    meta = meta_original,
    population = population,
    observation = observation,
    parameter = parameter,
    out_all = out_all
  )

  outdata
}
