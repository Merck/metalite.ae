# Copyright (c) 2024 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
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

#' Format exposure-adjusted AE summary
#'
#' @inheritParams extend_ae_specific_inference
#' @param display A character vector of measurement to be displayed:
#'   - `n`: Number of subjects exposed.
#'   - `total_exp`: Total exposure in person-time.
#'   - `events`: Number of AE.
#'   - `eaer`: Exposure adjusted event rate.
#'   - `total`: Total columns.
#' @param digits_total_exp A numeric value of number of digits for total exposure value.
#' @param digits_eaer A numeric value of number of digits for exposure-adjusted event rate.
#' @param mock A boolean value to display mock table.
#'
#' @return A list of analysis raw datasets.
#'
#' @export
#'
#' @examples
#' meta <- meta_ae_example()
#'
#' outdata <- meta |>
#'   prepare_ae_summary(
#'     population = "apat",
#'     observation = "wk12",
#'     parameter = "any;ser;rel"
#'   ) |>
#'   extend_ae_summary_eaer(adj_unit = "month")
#' tbl <- outdata |>
#'   format_ae_exp_adj()
#' head(tbl$tbl)
format_ae_exp_adj <- function(outdata,
                               display = c("n", "total_exp", "events", "eaer", "total"),
                               digits_total_exp = 2,
                               digits_eaer = 2,
                               mock = FALSE) {
  display <- tolower(display)
  display <- match.arg(display,
                       c("n", "total_exp", "events","eaer", "total"),
                       several.ok = TRUE
  )

  # Add "n"
  display <- unique(c("n", display))

  # Report Missing columns
  display_col <- setdiff(
    names(outdata),
    c(
      "meta", "population", "observation", "parameter",
      "order", "group", "name", "n_pop"
    )
  )
  display_missing <- setdiff(display, c(display_col, "total"))

  # Define total column
  display_total <- "total" %in% display
  index_total <- seq(ncol(outdata$n) - (!display_total))

  # Drop total column from outdata if it's not requested.
  outdata$group <- outdata$group[index_total]

  # Create output
  tbl <- list()

  # n
  tbl[["n"]] <- cbind(outdata$n[1, index_total], "-----")
  colnames(tbl[["n"]]) <- c(outdata$group, "row_label")
  col1 <- "Number of Participants exposed"

  if ("total_exp" %in% display) {
    if (is.null(outdata$total_exp)) {
      stop(
        "Please use `extend_ae_summary_eaer()` to total exposure.",
        call. = FALSE
      )
    }

    total_exp <- outdata$total_exp[, index_total]
    total_exp <- apply(total_exp, 2, fmt_pct, digits = digits_total_exp, pre = "", post = "")
    tbl[["total_exp"]] <- total_exp
    tbl[["total_exp"]]["row_label"] <- "-----"

    col1 <- c(col1, paste0("Total exposure in person-", outdata$adj_unit))
  }

  if ("events" %in% display) {
    if (is.null(outdata$event_num)) {
      stop(
        "Please use `extend_ae_summary_eaer()` to get events.",
        call. = FALSE
      )
    }

    events <- outdata$event_num[, index_total]
    tbl[["events"]] <- events
  }

  if ("eaer" %in% display) {
    if (is.null(outdata$total_exp)) {
      stop(
        "Please use `extend_ae_summary_eaer()` to get eaer",
        call. = FALSE
      )
    }

    eaer <- outdata$eaer[, index_total]
    eaer <- lapply(eaer, function(x, digits){
      x1 <- formatC(x, digits = digits, format = "f")
      paste0("(", x1, ")")
    }, digits = digits_eaer)
    eaer <- as.data.frame(do.call(cbind, eaer))

    if ("events" %in% display) {
      events_eaer <- matrix(
        mapply("paste", sep = " ", events, eaer),
        ncol = max(index_total)
      )
      colnames(events_eaer) <- colnames(eaer)
      tbl[["events"]] <- cbind(events_eaer, row_label = rep("Total events (rate)", length(unlist(strsplit(outdata$parameter, ";")))))
    } else {
      tbl[["events"]] <- cbind(eaer, row_label = rep("Total events (rate)", length(unlist(strsplit(outdata$parameter, ";")))))
    }
  }

  if (length(tbl[["events"]]) > 0){
    parameter_name <- setdiff(outdata$name, c("Participants in population", "with no adverse events"))
    parameter_name <- gsub("^with (|one or more )", "", parameter_name)
    parameter_name <- gsub("\\^a", "\\^b", parameter_name)
    col1 <- c(col1, parameter_name)
  }

  # Arrange Within Group information
  within_var <- names(tbl)[names(tbl) %in% c("n", "total_exp", "events")]
  within_tbl <- tbl[within_var]

  names(within_tbl) <- NULL
  n_within <- length(within_tbl)
  n_group <- ncol(tbl[["n"]])
  within_tbl <- do.call(rbind, within_tbl)
  within_tbl <- within_tbl[, as.vector(matrix(1:n_group,
                                              ncol = n_group, byrow = TRUE
  ))]

  # Align format_ae_specific
  res <- within_tbl

  # Transfer to Mock
  if (mock) {
    n_mock <- min(20, nrow(tbl[[1]]), na.rm = TRUE)
    res <- to_mock(res, n = nrow(tbl[[1]]))
  }

  res <- data.frame(name = col1, res)
  rownames(res) <- NULL

  if (mock) {
    res <- res[1:n_mock, ]
    outdata$order <- outdata$order[1:n_mock]
  }

  outdata$tbl <- res
  outdata$extend_call <- c(outdata$extend_call, match.call())

  outdata
}
