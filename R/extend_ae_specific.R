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

#' Add inference information for AE specific analysis
#'
#' @param outdata An `outdata` object created by [prepare_ae_specific()].
#' @param ci A numeric value for the percentile of confidence interval.
#' @param ... Other options passed on to [metalite.ae::rate_compare_sum()]
#'
#' @return A list of analysis raw datasets.
#'
#' @export
#'
#' @examples
#' meta <- meta_ae_example()
#' tbl <- prepare_ae_specific(meta,
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "rel"
#' ) |>
#'   extend_ae_specific_inference() |>
#'   format_ae_specific(display = c("n", "prop", "diff", "diff_ci"))
#' head(tbl$tbl)
#'
#' # use other options passed on to [metalite.ae::rate_compare_sum()]
#' tbl <- prepare_ae_specific(meta,
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "rel"
#' ) |>
#'   extend_ae_specific_inference(eps = 1e-6, bisection = 200) |>
#'   format_ae_specific(display = c("n", "prop", "diff", "diff_ci"))
#' head(tbl$tbl)
extend_ae_specific_inference <- function(outdata,
                                         ...,
                                         ci = 0.95) {
  res <- outdata

  if (!(is.numeric(ci) && length(ci) == 1 && (0 <= ci && ci <= 1))) {
    stop("`ci` is", ci, ". Please choose a number 0 >= ci >= 1.", call. = FALSE)
  }

  n_row <- nrow(res$n)
  ref <- res$reference_group
  grp <- (seq_along(res$group))[-c(ref, length(res$group))]

  ci_lower <- list()
  ci_upper <- list()
  p <- list()

  bind_rows2 <- utils::getFromNamespace("bind_rows2", ns = "metalite")

  for (iter in seq_along(grp)) {
    index <- grp[iter]
    x0 <- res$n[[ref]]
    x1 <- res$n[[index]]
    n0 <- rep(res$n_pop[[ref]], n_row)
    n1 <- rep(res$n_pop[[index]], n_row)

    # Calculate confidence interval
    tmp <- list()
    for (i in seq_along(x0)) {
      tmp[[i]] <- rate_compare_sum(
        x0 = x0[i],
        x1 = x1[i],
        n0 = n0[i],
        n1 = n1[i],
        alpha = 1 - ci,
        ...
      )
    }
    tmp <- bind_rows2(tmp)
    ci_lower[[iter]] <- tmp$lower
    ci_upper[[iter]] <- tmp$upper
    p[[iter]] <- tmp$p
  }

  ci_lower <- data.frame(do.call(cbind, ci_lower)) * 100
  names(ci_lower) <- paste0("lower_", grp)

  ci_upper <- data.frame(do.call(cbind, ci_upper)) * 100
  names(ci_upper) <- paste0("upper_", grp)

  p <- data.frame(do.call(cbind, p))
  names(p) <- paste0("p_", grp)

  res$ci_lower <- ci_lower
  res$ci_upper <- ci_upper
  res$ci_level <- ci
  res$p <- p
  res$extend_call <- c(res$call, match.call())
  res
}

#' Add average duration information for AE specific analysis
#'
#' @param outdata An `outdata` object created by [prepare_ae_specific()].
#' @param duration_var A character value of variable name for adverse event duration.
#' @param duration_unit A character value of adverse event duration unit.
#'
#' @return A list of analysis raw datasets.
#'
#' @export
#'
#' @examples
#' meta <- meta_ae_example()
#' tbl <- prepare_ae_specific(meta,
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "rel"
#' ) |>
#'   extend_ae_specific_duration(duration_var = "ADURN") |>
#'   format_ae_specific(display = c("n", "prop", "dur"))
#' head(tbl$tbl)
extend_ae_specific_duration <- function(outdata,
                                        duration_var,
                                        duration_unit = "Day") {
  meta <- outdata$meta

  if (!((length(duration_var) == 1) && is.character(duration_var))) {
    stop(
      "`duration_var` is ", duration_var,
      ". `duration_var` must be a string.",
      call. = FALSE
    )
  }

  if (!(duration_var %in% names(outdata$meta$data_observation))) {
    stop(duration_var, "does not exist in outdata.", call. = FALSE)
  }

  population <- outdata$population
  observation <- outdata$observation
  parameter <- outdata$parameter

  # Obtain variables
  pop_var <- collect_adam_mapping(meta, population)$var
  obs_var <- collect_adam_mapping(meta, observation)$var
  obs_dur <- duration_var
  par_var <- collect_adam_mapping(meta, parameter)$var
  par_soc <- collect_adam_mapping(meta, parameter)$soc

  # Obtain data
  pop <- collect_population_record(meta, population, var = pop_var)
  obs <- collect_observation_record(meta, population, observation, parameter,
    var = unique(c(obs_var, par_var, par_soc, obs_dur))
  )
  # Obtain variable name
  pop_id <- collect_adam_mapping(meta, population)$id
  obs_id <- collect_adam_mapping(meta, observation)$id

  pop_group <- collect_adam_mapping(meta, population)$group
  obs_group <- collect_adam_mapping(meta, observation)$group

  # Ensure group is a factor
  if (!"factor" %in% class(pop[[pop_group]])) {
    warning("In population level data, force group variable '", pop_group, "' be a factor")
    pop[[pop_group]] <- factor(pop[[pop_group]])
  }

  if (!"factor" %in% class(pop[[pop_group]])) {
    warning("In observation level data, force group variable '", obs_group, "' be a factor")
    obs[[obs_group]] <- factor(obs[[obs_group]], levels = levels(pop[[pop_group]]))
  }

  # Add a total group to display total column
  if (nrow(pop) == 0) {
    levels(pop[[pop_group]]) <- c(levels(pop[[pop_group]]), "Total")
  } else {
    pop_total <- pop
    pop_total[[pop_group]] <- "Total"
    pop <- rbind(pop, pop_total)
  }
  if (nrow(obs) == 0) {
    levels(obs[[obs_group]]) <- c(levels(obs[[obs_group]]), "Total")
  } else {
    obs_total <- obs
    obs_total[[obs_group]] <- "Total"
    obs <- rbind(obs, obs_total)
  }

  # Group information
  u_group <- levels(pop[[pop_group]])
  n_group <- length(u_group)

  # Overall duration
  obs_duration <- avg_duration(obs[[obs_id]], obs[[obs_group]], obs[[obs_dur]])
  obs_order <- 1e2

  soc_duration <- avg_duration(obs[[obs_id]], obs[[obs_group]], obs[[obs_dur]], obs[[par_soc]])
  soc_order <- outdata$order[outdata$order %% 1e3 == 0]
  soc_order <- soc_order[order(outdata$name[outdata$order %% 1e3 == 0])]

  par_duration <- avg_duration(obs[[obs_id]], obs[[obs_group]], obs[[obs_dur]], obs[[par_var]])
  par_order <- outdata$order[outdata$order > 1e3 & outdata$order %% 1e3 > 0]
  par_order <- par_order[order(outdata$name[outdata$order > 1e3 & outdata$order %% 1e3 > 0])]

  avg <- obs_duration$avg
  se <- obs_duration$se

  if (length(soc_order) > 0) {
    avg <- rbind(avg, soc_duration$avg)
    se <- rbind(se, soc_duration$se)
  }

  if (length(par_order) > 0) {
    avg <- rbind(avg, par_duration$avg)
    se <- rbind(se, par_duration$se)
  }

  # Define order and add a blank row
  index <- c(obs_order, soc_order, par_order)
  blank_order <- setdiff(outdata$order, index)
  blank_row <- data.frame(matrix(NA, nrow = length(blank_order), ncol = n_group))
  names(blank_row) <- names(avg)
  index <- c(index, blank_order)

  avg <- rbind(avg, blank_row)[order(index), ]
  names(avg) <- paste0("dur_", seq_len(ncol(avg)))

  se <- rbind(se, blank_row)[order(index), ]
  names(se) <- paste0("dur_se", seq_len(ncol(se)))

  outdata$dur <- avg
  outdata$dur_se <- se
  outdata$extend_call <- c(outdata$extend_call, match.call())

  outdata
}

#' Add average number of events information for AE specific analysis
#'
#' @param outdata An `outdata` object created by [prepare_ae_specific()].
#'
#' @return A list of analysis raw datasets.
#'
#' @export
#'
#' @examples
#' meta <- meta_ae_example()
#' tbl <- prepare_ae_specific(meta,
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "rel"
#' ) |>
#'   extend_ae_specific_events() |>
#'   format_ae_specific(display = c("n", "prop", "events_avg"))
#' head(tbl$tbl)
extend_ae_specific_events <- function(outdata) {
  meta <- outdata$meta

  population <- outdata$population
  observation <- outdata$observation
  parameter <- outdata$parameter

  # Obtain variables
  pop_var <- collect_adam_mapping(meta, population)$var
  obs_var <- collect_adam_mapping(meta, observation)$var
  par_var <- collect_adam_mapping(meta, parameter)$var
  par_soc <- collect_adam_mapping(meta, parameter)$soc

  # Obtain data
  pop <- collect_population_record(meta, population, var = pop_var)
  obs <- collect_observation_record(meta, population, observation, parameter,
    var = unique(c(obs_var, par_var, par_soc))
  )
  # Obtain variable name
  pop_id <- collect_adam_mapping(meta, population)$id
  obs_id <- collect_adam_mapping(meta, observation)$id

  pop_group <- collect_adam_mapping(meta, population)$group
  obs_group <- collect_adam_mapping(meta, observation)$group

  # Ensure group is a factor
  if (!"factor" %in% class(pop[[pop_group]])) {
    warning("In population level data, force group variable '", pop_group, "' be a factor")
    pop[[pop_group]] <- factor(pop[[pop_group]])
  }

  if (!"factor" %in% class(pop[[pop_group]])) {
    warning("In observation level data, force group variable '", obs_group, "' be a factor")
    obs[[obs_group]] <- factor(obs[[obs_group]], levels = levels(pop[[pop_group]]))
  }

  # Add a total group to display total column
  if (nrow(pop) == 0) {
    levels(pop[[pop_group]]) <- c(levels(pop[[pop_group]]), "Total")
  } else {
    pop_total <- pop
    pop_total[[pop_group]] <- "Total"
    pop <- rbind(pop, pop_total)
  }
  if (nrow(obs) == 0) {
    levels(obs[[obs_group]]) <- c(levels(obs[[obs_group]]), "Total")
  } else {
    obs_total <- obs
    obs_total[[obs_group]] <- "Total"
    obs <- rbind(obs, obs_total)
  }

  # Group information
  u_group <- levels(pop[[pop_group]])
  n_group <- length(u_group)

  # Overall duration
  obs_events <- avg_event(obs[[obs_id]], obs[[obs_group]])
  obs_order <- 1e2

  soc_events <- avg_event(obs[[obs_id]], obs[[obs_group]], obs[[par_soc]])
  soc_events$count <- replace(soc_events$count, is.na(soc_events$count), 0)
  soc_order <- outdata$order[outdata$order %% 1e3 == 0]
  soc_order <- soc_order[order(outdata$name[outdata$order %% 1e3 == 0])]

  par_events <- avg_event(obs[[obs_id]], obs[[obs_group]], obs[[par_var]])
  par_events$count <- replace(par_events$count, is.na(par_events$count), 0)
  par_order <- outdata$order[outdata$order > 1e3 & outdata$order %% 1e3 > 0]
  par_order <- par_order[order(outdata$name[outdata$order > 1e3 & outdata$order %% 1e3 > 0])]

  avg <- obs_events$avg
  se <- obs_events$se
  count <- obs_events$count

  if (length(soc_order) > 0) {
    avg <- rbind(avg, soc_events$avg)
    se <- rbind(se, soc_events$se)
    count <- rbind(count, soc_events$count)
  }

  if (length(par_order) > 0) {
    avg <- rbind(avg, par_events$avg)
    se <- rbind(se, par_events$se)
    count <- rbind(count, par_events$count)
  }

  # Define order and add a blank row
  index <- c(obs_order, soc_order, par_order)
  blank_order <- setdiff(outdata$order, index)
  blank_row <- data.frame(matrix(NA, nrow = length(blank_order), ncol = n_group))
  names(blank_row) <- names(avg)
  index <- c(index, blank_order)

  avg <- rbind(avg, blank_row)[order(index), ]
  names(avg) <- paste0("eventsavg_", seq_len(ncol(avg)))

  se <- rbind(se, blank_row)[order(index), ]
  names(se) <- paste0("events_se", seq_len(ncol(se)))

  count <- rbind(count, blank_row)[order(index), ]
  names(count) <- paste0("eventscount_", seq_len(ncol(count)))

  # fill NA value with zero
  outdata$events_avg <- avg
  outdata$events_se <- se
  # fill NA value with zero
  outdata$events_count <- count
  outdata$extend_call <- c(outdata$extend_call, match.call())

  outdata
}

#' Add subgroup analysis in AE specific analysis
#'
#' @param outdata An `outdata` object created by [prepare_ae_specific()].
#' @param subgroup_var a character string for subgroup variable name
#'
#' @return A list of analysis raw datasets.
#' @export
#' @examples
#' meta <- meta_ae_example()
#' tbl <- prepare_ae_specific(meta,
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "rel"
#' ) |>
#'   extend_ae_specific_subgroup(subgroup_var = "SEX")
extend_ae_specific_subgroup <- function(outdata, subgroup_var) {
  outdata$subgroup_var <- subgroup_var
  outdata$subgroup_header <- c(outdata$meta$population[[outdata$population]]$group, outdata$subgroup_var)
  meta_subgroup <- metalite::meta_split(outdata$meta, outdata$subgroup_header[2])
  outdata$components <- c("soc", "par")

  subgrp_out <- lapply(meta_subgroup, function(subgroup) {
    prepare_ae_specific(
      meta = outdata$meta,
      population = outdata$population,
      observation = outdata$observation,
      parameter = outdata$parameter,
      components = outdata$components
    )
  })

  out_all <- subgrp_out
  out_all$Total <- outdata

  return(out_all)
}
