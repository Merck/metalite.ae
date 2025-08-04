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
#' @param components A character vector of components name.
#' @param reference_group An integer to indicate reference group.
#'   Default is 2 if there are 2 groups, otherwise, the default is 1.
#'
#' @return A list of analysis datasets needed for AE specific analysis.
#'
#' @import metalite
#'
#' @export
#'
#' @examples
#' meta <- meta_ae_example()
#' str(prepare_ae_specific(meta, "apat", "wk12", "rel"))
#'
#' # Allow to extract each components
#' prepare_ae_specific(meta, "apat", "wk12", "rel", components = NULL)$data
#' prepare_ae_specific(meta, "apat", "wk12", "rel", components = "soc")$data
#' prepare_ae_specific(meta, "apat", "wk12", "rel", components = "par")$data
prepare_ae_specific <- function(meta,
                                population,
                                observation,
                                parameter,
                                components = c("soc", "par"),
                                reference_group = NULL) {
  # Obtain variables
  pop_var <- collect_adam_mapping(meta, population)$var
  obs_var <- collect_adam_mapping(meta, observation)$var
  par_var <- collect_adam_mapping(meta, parameter)$var
  par_soc <- collect_adam_mapping(meta, parameter)$soc

  # Obtain Data
  pop <- collect_population_record(meta, population, var = pop_var)
  obs <- collect_observation_record(meta, population, observation, parameter,
    var = unique(c(obs_var, par_var, par_soc))
  )
  # Obtain variable name
  pop_id <- collect_adam_mapping(meta, population)$id
  obs_id <- collect_adam_mapping(meta, observation)$id

  pop_group <- collect_adam_mapping(meta, population)$group
  obs_group <- collect_adam_mapping(meta, observation)$group

  # Check if the grouping variable is missing
  if (any(is.na(pop[[pop_group]]))) {
    stop(
      paste0(
        "There are >= 1 subjects with missing grouping variable '", pop_group,
        "' in the population dataset."
      ),
      call. = FALSE
    )
  }
  if (any(is.na(obs[[obs_group]]))) {
    stop(
      paste0(
        "There are >= 1 subjects with missing grouping variable '", obs_group,
        "' in the observation dataset."
      ),
      call. = FALSE
    )
  }

  # Ensure group is a factor
  if (!"factor" %in% class(pop[[pop_group]])) {
    warning("In population level data, force group variable '", pop_group, "' be a factor")
    pop[[pop_group]] <- factor(pop[[pop_group]])
  }

  if (!"factor" %in% class(obs[[obs_group]])) {
    warning("In observation level data, force group variable '", obs_group, "' be a factor")
    obs[[obs_group]] <- factor(obs[[obs_group]])
    levels(obs[[obs_group]]) <- levels(pop[[pop_group]])
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

  # Define reference group
  if (is.null(reference_group)) {
    reference_group <- ifelse(n_group - 1 == 2, 2, 1)
  }

  # Number of subjects
  pop_n <- metalite::n_subject(pop[[pop_id]], pop[[pop_group]])
  obs_n <- metalite::n_subject(obs[[obs_id]], obs[[obs_group]])
  obs_n <- rbind(obs_n, pop_n - obs_n)

  # Define Population section
  pop_n$name <- "Participants in population"
  pop_n$order <- 1

  # Define Observation section
  obs_n$name <- c(
    "with one or more {tolower(term1)} adverse events {tolower(term2)}",
    "with no {tolower(term1)} adverse events {tolower(term2)}"
  )
  obs_n$name <- vapply(obs_n$name, glue::glue_data, .x = collect_adam_mapping(meta, parameter), FUN.VALUE = character(1))
  # Remove duplicate space
  obs_n$name <- gsub("^ *|(?<= ) | *$", "", obs_n$name, perl = TRUE)

  obs_n$order <- 1e2 * seq_len(nrow(obs_n))

  counts <- table(do.call(paste, obs[c(par_var, par_soc)]))
  max_per_soc <- tapply(counts, sapply(strsplit(names(counts), " "), function(x) paste(tail(x, length(par_soc)), collapse = " ")), max)
  overall_max <- max(max_per_soc)

  ck <- 10^(floor(log10(overall_max)) + 2)

  # Define SOC section
  if ("soc" %in% components && nrow(obs) > 0) {
    soc_n <- metalite::n_subject(obs[[obs_id]], obs[[obs_group]], obs[[par_soc]],
      na = "NULL"
    )

    soc_n[[par_soc]] <- soc_n$name
    soc_n[[par_var]] <- soc_n$name
    soc_n$order <- ck * seq_len(nrow(soc_n))
    soc_n$name <- to_sentence(soc_n$name)
    soc_n$soc_name <- soc_n$name
  } else {
    soc_n <- NULL
  }

  # Define AE term section
  if ("par" %in% components && nrow(obs) > 0) {
    u_soc <- unique(obs[order(obs[[par_soc]]), c(par_soc, par_var)])

    par_n <- metalite::n_subject(obs[[obs_id]], obs[[obs_group]], obs[[par_var]],
      na = "NULL"
    )

    par_n[[par_var]] <- par_n$name
    par_n <- merge(u_soc, par_n, all.y = TRUE)
    par_n$order <- ck * as.numeric(factor(par_n[[par_soc]])) + seq_len(nrow(par_n))
    par_n$order[is.na(par_n$order)] <- (if (!all(is.na(soc_n$order))) max(soc_n$order, na.rm = TRUE) else -Inf) + 1
    par_n$name <- to_sentence(par_n$name)
    par_n$soc_name <- par_n[[par_soc]]
  } else {
    par_n <- NULL
  }

  # Create blank row
  col <- c("name", u_group, "order")
  blank_row <- data.frame(name = "", matrix(NA, nrow = 1, ncol = n_group), order = 900)
  names(blank_row) <- names(pop_n[, col])

  # Combine count values
  tbl0 <- rbind(pop_n[, col], obs_n[, col], blank_row)
  tbl0$soc_name <- NA
  tbl <- rbind(par_n[, c(col, "soc_name")], soc_n[, c(col, "soc_name")])
  tbl <- rbind(tbl0, tbl)
  tbl <- tbl[order(tbl$order), ]
  soc_name <- tbl$soc_name
  tbl <- tbl[, !(names(tbl) %in% "soc_name")]


  # Calculate Proportion
  tbl_num <- tbl[, u_group]
  tbl_dom <- matrix(as.numeric(pop_n[, u_group]), ncol = n_group, nrow = nrow(tbl), byrow = TRUE)
  tbl_rate <- tbl_num / tbl_dom * 100
  tbl_rate[1, ] <- NA
  names(tbl_num) <- paste0("n_", 1:n_group)
  colnames(tbl_rate) <- paste0("prop_", 1:n_group)

  # Calculate risk difference
  tbl_diff <- tbl_rate - tbl_rate[, reference_group]
  colnames(tbl_diff) <- paste0("diff_", 1:n_group)
  tbl_diff <- tbl_diff[-c(reference_group, n_group)]

  # Return value
  metalite::outdata(meta, population, observation, parameter,
    n = tbl_num, order = tbl$order, group = u_group, reference_group = reference_group,
    prop = tbl_rate, diff = tbl_diff,
    n_pop = tbl_num[1, ],
    name = tbl$name,
    soc_name = soc_name,
    components = components,
    prepare_call = match.call()
  )
}
