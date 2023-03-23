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

#' Count number of unique subjects
#'
#' @param id A character vector of subject ID.
#' @param group A factor vector of group name.
#' @param par A character vector of parameter name.
#' @param use_na A character value for whether to include `NA` values
#'   in the table. See the `useNA` argument in [base::table()] for details.
#'
#' @noRd
#'
#' @examples
#' library(r2rtf)
#'
#' r2rtf_adae$TRTA <- factor(r2rtf_adae$TRTA)
#' r2rtf_adae$SEX[1:5] <- NA
#'
#' metalite.ae:::n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA)
#' metalite.ae:::n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA, r2rtf_adae$SEX)
#' metalite.ae:::n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA, r2rtf_adae$SEX, use_na = "always")
n_subject <- function(id, group, par = NULL, use_na = c("ifany", "no", "always")) {
  use_na <- match.arg(use_na)

  if ("factor" %in% class(group)) {
    u_group <- c(as.character(levels(group)), "Missing")
  } else {
    stop("n_subject: group variable must be a factor.", call. = FALSE)
  }

  if (is.null(par)) {
    db <- data.frame(id = id, group = group)
    res <- table(unique(db)[["group"]], useNA = use_na)

    n_row <- nrow(res)
    res <- data.frame(t(as.vector(res)))
    names(res) <- c(u_group[1:n_row])
  } else {
    db <- data.frame(id = id, group = group, par = par)
    res <- table(unique(db)[, c("group", "par")], useNA = use_na)
    name <- colnames(res)
    name[is.na(name)] <- "Missing"

    n_row <- nrow(res)
    n_col <- ncol(res)
    res <- data.frame(name = name[1:n_col], matrix(res, ncol = n_row, byrow = TRUE))
    names(res) <- c("name", u_group[1:n_row])
  }

  res
}

#' Average number of events
#'
#' @inheritParams n_subject
#'
#' @importFrom stats sd
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
    avg <- vapply(split(res, res$Var2), function(x) mean(x$Freq, na.rm = TRUE), FUN.VALUE = numeric(1))
    se <- vapply(split(res, res$Var2), function(x) sd(x$Freq, na.rm = TRUE) / sqrt(nrow(x)), FUN.VALUE = numeric(1))
  } else {
    db <- data.frame(id = id, group = group, par = par)
    res <- table(db$id, db$group, db$par)
    res <- data.frame(res)

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
          se = sd(X$n, na.rm = TRUE) / sqrt(nrow(X))
        )
      }) |>
      do.call(what = rbind) |>
      reshape(timevar = "group", idvar = "par", direction = "wide")

    rownames(tmp) <- NULL

    avg <- cbind(par = tmp$par, tmp[, grepl(names(tmp), pattern = "^avg")])
    names(avg) <- sub(names(avg), pattern = "avg\\.", replacement = "")
    avg <- avg[order(avg$par), ]
    avg <- avg[, u_group]

    se <- cbind(par = tmp$par, tmp[, grepl(names(tmp), pattern = "^se")])
    names(se) <- sub(names(se), pattern = "se\\.", replacement = "")
    se <- se[order(se$par), ]
    se <- se[, u_group]
  }

  list(avg = avg, se = se)
}

#' Average number of events
#'
#' @inheritParams n_subject
#' @param dur A numeric vector of AE duration.
#'
#' @importFrom stats sd
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

    # Compute average and se by treatment group.
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

    tmp2 <- split(db, ~ group + par, drop = TRUE) |>
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

    rownames(tmp2) <- NULL

    avg <- cbind(par = tmp2$par, tmp2[, grepl(names(tmp2), pattern = "^avg")])
    names(avg) <- sub(names(avg), pattern = "avg\\.", replacement = "")
    avg <- avg[order(avg$par), ]
    avg <- avg[, u_group]

    se <- cbind(par = tmp2$par, tmp2[, grepl(names(tmp2), pattern = "^se")])
    names(se) <- sub(names(se), pattern = "se\\.", replacement = "")
    se <- se[order(se$par), ]
    se <- se[, u_group]
  }

  list(avg = avg, se = se)
}
