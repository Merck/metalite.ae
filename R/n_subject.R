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

#' Count number of unique subjects
#'
#' @param id a character vector of subject id
#' @param group a factor vector of group name
#' @param par a character vector of parameter name
#'
#' @examples
#' library(r2rtf)
#' r2rtf_adae$TRTA <- factor(r2rtf_adae$TRTA)
#' metalite.ae:::n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA)
#' metalite.ae:::n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA, r2rtf_adae$AEDECOD)
n_subject <- function(id, group, par = NULL) {
  if ("factor" %in% class(group)) {
    u_group <- as.character(levels(group))
  } else {
    stop("n_subject: group variable must be a factor")
  }

  if (is.null(par)) {
    db <- data.frame(id = id, group = group)
    res <- table(unique(db)[["group"]])
    res <- data.frame(t(as.vector(res)))
    names(res) <- c(u_group)
  } else {
    db <- data.frame(id = id, group = group, par = par)
    res <- table(unique(db)[, c("group", "par")])
    name <- colnames(res)
    res <- data.frame(name = name, matrix(res, ncol = length(u_group), byrow = TRUE))
    names(res) <- c("name", u_group)
  }

  res
}

#' Average number of events
#'
#' @inheritParams n_subject
#'
#' @importFrom dplyr count group_by summarise select starts_with n
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' # library(r2rtf)
#' # r2rtf_adae$TRTA <- factor(r2rtf_adae$TRTA)
#' # metalite.ae:::avg_event(r2rtf_adae$USUBJID, r2rtf_adae$TRTA)
#' # metalite.ae:::avg_event(r2rtf_adae$USUBJID, r2rtf_adae$TRTA, r2rtf_adae$AEDECOD)
avg_event <- function(id, group, par = NULL) {
  if ("factor" %in% class(group)) {
    u_group <- as.character(levels(group))
  } else {
    stop("avg_event: group variable must be a factor")
  }

  if (is.null(par)) {
    db <- data.frame(id = id, group = group)
    res <- table(db$id, db$group)
    res <- data.frame(res)
    avg <- vapply(split(res, res$Var2), function(x) mean(x$Freq, na.rm = TRUE), FUN.VALUE = numeric(1))
    se <- vapply(split(res, res$Var2), function(x) sd(x$Freq, na.rm = TRUE) / sqrt(nrow(x)), FUN.VALUE = numeric(1))
  } else {
    db <- data.frame(id = id, group = group, par = par)
    res <- table(db$id, db$group, db$par)
    res <- data.frame(res)

    tmp <- db |>
      count(group, par, id) |>
      group_by(group, par) |>
      summarise(avg = mean(n, na.rm = TRUE), se = sd(n, na.rm = TRUE) / sqrt(n())) |>
      tidyr::pivot_wider(id_cols = par, names_from = group, values_from = c("avg", "se"))

    avg <- tmp |>
      select(starts_with("avg")) |>
      as.data.frame()
    names(avg) <- u_group

    se <- tmp |>
      select(starts_with("se")) |>
      as.data.frame()
    names(se) <- u_group
  }

  list(avg = avg, se = se)
}

#' Average number of events
#'
#' @inheritParams n_subject
#' @param dur a numeric vector of AE duration
#'
#' @importFrom dplyr n
avg_duration <- function(id, group, dur, par = NULL) {
  if ("factor" %in% class(group)) {
    u_group <- as.character(levels(group))
  } else {
    stop("avg_event: group variable must be a factor")
  }

  if (is.null(par)) {
    db <- data.frame(id = id, group = group, dur = dur)
    res <- db |>
      group_by(group) |>
      summarise(
        avg = mean(dur, na.rm = TRUE),
        se = sd(dur, na.rm = TRUE) / sqrt(n())
      )

    avg <- res$avg
    names(avg) <- u_group

    se <- res$se
    names(se) <- u_group
  } else {
    db <- data.frame(id = id, group = group, dur = dur, par = par)

    tmp <- db |>
      group_by(group, par) |>
      summarise(
        avg = mean(dur, na.rm = TRUE),
        se = sd(dur, na.rm = TRUE) / sqrt(n())
      ) |>
      tidyr::pivot_wider(id_cols = par, names_from = group, values_from = c("avg", "se"))

    avg <- tmp |>
      select(starts_with("avg")) |>
      as.data.frame()
    names(avg) <- u_group

    se <- tmp |>
      select(starts_with("se")) |>
      as.data.frame()
    names(se) <- u_group
  }

  list(avg = avg, se = se)
}
