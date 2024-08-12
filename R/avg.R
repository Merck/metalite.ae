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

#' Average number of events
#'
#' Calculates average number of records per group and, if requested, parameter.
#' Returns a list of means and SD of counts of records per group, ordered in
#' the same way as the input datasets.
#'
#' @inheritParams n_subject
#'
#' @importFrom stats sd reshape
#'
#' @return A list of means and SD of counts of records per group.
#'
#' @noRd
#'
#' @examples
#' library(r2rtf)
#'
#' r2rtf_adae$TRTA <- factor(r2rtf_adae$TRTA)
#' metalite.ae:::avg_event(r2rtf_adae$USUBJID, r2rtf_adae$TRTA)
#' metalite.ae:::avg_event(r2rtf_adae$USUBJID, r2rtf_adae$TRTA, r2rtf_adae$AEDECOD)
avg_event <- function(id, group, par = NULL) {
  if ("factor" %in% class(group)) {
    u_group <- as.character(levels(group))
  } else {
    stop("avg_event: group variable must be a factor.", call. = FALSE)
  }

  if (is.null(par)) {
    db <- data.frame(id = id, group = group)
    res <- table(db$id, db$group)
    res <- data.frame(res)
    avg <- vapply(split(res, res$Var2),
      function(x) mean(x$Freq, na.rm = TRUE),
      FUN.VALUE = numeric(1)
    )
    se <- vapply(split(res, res$Var2),
      function(x) sd(x$Freq, na.rm = TRUE) / sqrt(nrow(x)),
      FUN.VALUE = numeric(1)
    )
    count <- vapply(split(res, res$Var2),
      nrow,
      FUN.VALUE = numeric(1)
    )
  } else {
    db <- data.frame(id = id, group = group, par = par)

    # Count number of observations per group, par, and id
    tmp <- split(db, ~ group + par + id, drop = TRUE) |>
      lapply(FUN = function(X) {
        data.frame(
          group = unique(X$group),
          par = unique(X$par),
          n = nrow(X)
        )
      }) |>
      do.call(what = rbind) |>
      split(~ group + par, drop = TRUE) |>
      lapply(FUN = function(X) {
        data.frame(
          group = unique(X$group),
          par = unique(X$par),
          avg = mean(X$n, na.rm = TRUE),
          se = sd(X$n, na.rm = TRUE) / sqrt(nrow(X)),
          count = nrow(X)
        )
      }) |>
      do.call(what = rbind) |>
      # Spread to wide format so that group.statistics are column variables
      reshape(timevar = "group", idvar = "par", direction = "wide", new.row.names = NULL)

    # Sort the summarized data so that par is in the same order as input
    tmp <- merge(data.frame(par = unique(db$par)), tmp, by = "par", sort = FALSE)
    if (any(unique(tmp$par) != unique(db$par))) stop("sorting is broken, try again")

    # Remove row names
    rownames(tmp) <- NULL

    # Extract avg and se into separate datasets
    avg <- tmp[, grepl(names(tmp), pattern = "^avg")]
    names(avg) <- sub(names(avg), pattern = "avg\\.", replacement = "")
    # Reorder columns (group) to be as input
    avg <- avg[, u_group]

    se <- tmp[, grepl(names(tmp), pattern = "^se")]
    names(se) <- sub(names(se), pattern = "se\\.", replacement = "")
    # Reorder columns (group) to be as input
    se <- se[, u_group]

    count <- tmp[, grepl(names(tmp), pattern = "^count")]
    names(count) <- sub(names(count), pattern = "count\\.", replacement = "")
    count <- count[, u_group]
  }

  list(avg = avg, se = se, count = count)
}

#' Calculates average duration per group and, if requested, parameter.
#' Returns a list of means and sd of durations per group, ordered in
#' the same way as the input datasets.
#'
#' @inheritParams n_subject
#' @param dur A numeric vector of adverse event duration.
#'
#' @importFrom stats sd reshape
#'
#' @noRd
avg_duration <- function(id, group, dur, par = NULL) {
  if ("factor" %in% class(group)) {
    u_group <- as.character(levels(group))
  } else {
    stop("avg_event: group variable must be a factor.", call. = FALSE)
  }

  if (is.null(par)) {
    db <- data.frame(id = id, group = group, dur = dur)

    # Compute average and se of dur by treatment group
    res <- split(db, db$group) |>
      lapply(FUN = function(X) {
        data.frame(
          group = unique(X$group),
          avg = mean(X$dur, na.rm = TRUE),
          se = sd(X$dur, na.rm = TRUE) / sqrt(nrow(X))
        )
      }) |>
      do.call(what = rbind)

    avg <- res$avg
    names(avg) <- res$group

    se <- res$se
    names(se) <- res$group
  } else {
    db <- data.frame(id = id, group = group, dur = dur, par = par)

    # summarize dur by group and par
    tmp <- split(db, ~ group + par, drop = TRUE) |>
      lapply(FUN = function(X) {
        data.frame(
          group = unique(X$group),
          par = unique(X$par),
          avg = mean(X$dur, na.rm = TRUE),
          se = sd(X$dur, na.rm = TRUE) / sqrt(nrow(X))
        )
      }) |>
      do.call(what = rbind) |>
      reshape(timevar = "group", idvar = "par", direction = "wide")
    # Sort the summarized data so that par is in the same order as input
    tmp <- merge(data.frame(par = unique(db$par)), tmp, by = "par", sort = FALSE)
    if (any(unique(tmp$par) != unique(db$par))) stop("sorting is broken, try again")

    # Set row names to null
    rownames(tmp) <- NULL


    # Extract avg and se into separate datasets
    avg <- cbind(par = tmp$par, tmp[, grepl(names(tmp), pattern = "^avg")])
    names(avg) <- sub(names(avg), pattern = "avg\\.", replacement = "")
    # Reorder columns (group) to be as input
    avg <- avg[, u_group]

    se <- cbind(par = tmp$par, tmp[, grepl(names(tmp), pattern = "^se")])
    names(se) <- sub(names(se), pattern = "se\\.", replacement = "")
    # Reorder columns (group) to be as input
    se <- se[, u_group]
  }

  list(avg = avg, se = se)
}
