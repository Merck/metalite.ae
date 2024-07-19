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

#' Format AE specific analysis
#'
#' @inheritParams extend_ae_specific_inference
#' @param digits_prop A numeric value of number of digits for proportion value.
#' @param digits_ci A numeric value of number of digits for confidence interval.
#' @param digits_p A numeric value of number of digits for p-value.
#' @param digits_dur A numeric value of number of digits for average
#'   duration of adverse event.
#' @param digits_events A numeric value of number of digits for average of
#'   number of adverse events per subject.
#' @param display A character vector of measurement to be displayed:
#'   - `n`: Number of subjects with adverse event.
#'   - `prop`: Proportion of subjects with adverse event.
#'   - `total`: Total columns.
#'   - `diff`: Risk difference.
#'   - `diff_ci`: 95% confidence interval of risk difference using M&N method.
#'   - `diff_p`: p-value of risk difference using M&N method.
#'   - `dur`: Average of adverse event duration.
#'   - `events`: Average number of adverse event per subject.
#' @param filter_method A character value to specify how to filter rows:
#'  - `count`: Filtered based on participant count.
#'  - `percent`: Filtered based percent incidence.
#' @param filter_criteria A numeric value to display rows where at least
#'    one therapy group has a percent incidence or participant count
#'    greater than or equal to the specified value.
#'    If `filter_method` is `percent`, the value should be between 0 and 100.
#'    If `filter_method` is `count`, the value should be greater than 0.
#' @param sort_order A character value to specify sorting order:
#'  - `alphabetical`: Sort by alphabetical order.
#'  - `count_des`: Sort by count in descending order.
#'  - `count_asc`: Sort by count in ascending order.
#' @param sort_column A character value of `group` in `outdata` used to sort a table with.
#' @param mock A boolean value to display mock table.
#'
#' @return A list of analysis raw datasets.
#'
#' @export
#'
#' @examples
#' meta <- meta_ae_example()
#'
#' outdata <- prepare_ae_specific(meta,
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "rel"
#' )
#'
#' # Basic example
#' tbl <- outdata |>
#'   format_ae_specific()
#' head(tbl$tbl)
#'
#' # Filtering
#' tbl <- outdata |>
#'   format_ae_specific(
#'     filter_method = "percent",
#'     filter_criteria = 10
#'   )
#' head(tbl$tbl)
#'
#' # Display different measurements
#' tbl <- outdata |>
#'   extend_ae_specific_events() |>
#'   format_ae_specific(display = c("n", "prop", "events"))
#' head(tbl$tbl)
format_ae_specific <- function(outdata,
                               display = c("n", "prop", "total"),
                               digits_prop = 1,
                               digits_ci = 1,
                               digits_p = 3,
                               digits_dur = c(1, 1),
                               digits_events = c(1, 1),
                               filter_method = c("percent", "count"),
                               filter_criteria = 0,
                               sort_order = c("alphabetical", "count_des", "count_asc"),
                               sort_column = NULL,
                               mock = FALSE) {
  display <- tolower(display)
  display <- match.arg(display,
    c("n", "prop", "total", "diff", "diff_ci", "diff_p", "dur", "events"),
    several.ok = TRUE
  )
  filter_method <- match.arg(filter_method, c("percent", "count"))
  sort_order <- match.arg(sort_order, c("alphabetical", "count_des", "count_asc"))

  # Add "n"
  display <- unique(c("n", display))

  # Report Missing columns
  display_col <- setdiff(
    names(outdata),
    c(
      "meta", "population", "observation", "parameter",
      "order", "group", "reference_group", "name", "n_pop"
    )
  )
  display_missing <- setdiff(display, c(display_col, "total"))

  # TODO: check missing components
  # if(length(display_missing) > 0){
  #   stop("Missing component in outdata: ", paste(display_missing, collapse = "; "), call. = FALSE)
  # }

  # Define total column
  display_total <- "total" %in% display
  index_total <- seq(ncol(outdata$n) - (!display_total))

  # Drop total column from outdata if it's not requested.
  outdata$group <- outdata$group[index_total]

  # Create output
  tbl <- list()

  # n
  tbl[["n"]] <- outdata$n[, index_total]

  if ("prop" %in% display) {
    prop <- outdata$prop[, index_total]
    prop <- apply(prop, 2, fmt_pct, digits = digits_prop, pre = "(", post = ")")
    tbl[["prop"]] <- prop
  }

  if ("diff" %in% display) {
    # diff <- outdata$diff[, index_total]
    diff <- apply(outdata$diff, 2, fmt_est, digits = digits_prop)
    tbl[["diff"]] <- diff
  }

  if ("diff_ci" %in% display) {
    if (is.null(outdata$ci_lower)) {
      stop(
        "Please use `extend_ae_specific_inference()` to get ci.",
        call. = FALSE
      )
    }

    ci <- outdata$ci_lower * NA
    names(ci) <- gsub("lower", "ci", names(ci))
    for (i in seq_len(ncol(outdata$ci_lower))) {
      lower <- outdata$ci_lower[[i]]
      upper <- outdata$ci_upper[[i]]
      ci[, i] <- fmt_ci(lower, upper, digits = digits_ci)
    }
    tbl[["diff_ci"]] <- ci
  }

  if ("diff_p" %in% display) {
    if (is.null(outdata$p)) {
      stop(
        "Please use `extend_ae_specific_inference()` to get p-values.",
        call. = FALSE
      )
    }

    p <- apply(outdata$p, 2, fmt_pval, digits = digits_p)
    tbl[["diff_p"]] <- p
  }

  if ("dur" %in% display) {
    if (is.null(outdata$dur)) {
      stop(
        "Please use `extend_ae_specific_duration()` to get duration.",
        call. = FALSE
      )
    }

    dur <- outdata$dur[, index_total] * NA
    for (i in seq(index_total)) {
      m <- outdata$dur[[i]]
      se <- outdata$dur_se[[i]]
      dur[, i] <- fmt_est(m, se, digits = digits_dur)
    }
    tbl[["dur"]] <- dur
  }

  if ("events" %in% display) {
    if (is.null(outdata$events)) {
      stop(
        "Please use `extend_ae_specific_events()` to get events.",
        call. = FALSE
      )
    }

    events <- outdata$events[, index_total] * NA
    for (i in seq(index_total)) {
      m <- outdata$events[[i]]
      se <- outdata$events_se[[i]]
      events[, i] <- fmt_est(m, se, digits = digits_dur)
    }
    tbl[["events"]] <- events
  }

  # Arrange Within Group information
  within_var <- names(tbl)[names(tbl) %in% c("n", "prop", "dur", "events")]
  within_tbl <- tbl[within_var]

  names(within_tbl) <- NULL
  n_within <- length(within_tbl)
  n_group <- ncol(tbl[["n"]])
  within_tbl <- do.call(cbind, within_tbl)
  within_tbl <- within_tbl[, as.vector(matrix(1:(n_group * n_within),
    ncol = n_group, byrow = TRUE
  ))]

  # Arrange group comparison information (diff, diff_ci, diff_p).
  between_var <- names(tbl)[names(tbl) %in% c("diff", "diff_ci", "diff_p")]

  ## If there are any comparison variables selected, order their columns appropriately.
  res <- if (length(between_var) != 0) {
    between_tbl <- tbl[between_var]
    n_between <- length(between_tbl)
    # Number of comparison columns is always: treatment groups - 1 - total column (1 or 0).
    n_group_btw <- n_group - 1 - display_total
    names(between_tbl) <- NULL
    # Bind all the comparison columns together.
    between_tbl <- do.call(cbind, between_tbl)
    # Reorder the comparison columns.
    between_tbl <- between_tbl[, as.vector(matrix(1:(n_group_btw * n_between),
      ncol = n_group_btw, byrow = TRUE
    ))]
    data.frame(within_tbl, between_tbl)
  } else {
    within_tbl
  }

  res <- data.frame(name = outdata$name, res)

  # Additional options for AE specific
  if ("prepare_ae_specific" %in% as.character(outdata$prepare_call)) {
    if ("soc_name" %in% names(outdata)) {
      soc_name <- outdata$soc_name
    }

    # Filtering by criteria
    if (filter_criteria > 0) {
      if (filter_method == "percent") {
        # Round before filtering
        filter_index <- round(outdata$prop[, index_total], digits_prop)
      } else {
        filter_index <- outdata$n[, index_total]
      }

      # Create filter text
      filter_text <- paste0("filter_index[", index_total, "] >= ", filter_criteria, collapse = " | ")
      filter_logic <- eval(parse(text = filter_text))
      # Keep fixed rows
      filter_logic[1:4] <- rep(TRUE, 4)
      res <- res[filter_logic, ]
      outdata$order <- outdata$order[filter_logic]
      soc_name <- soc_name[filter_logic]
    }

    # Get index of sort column
    if (sort_order %in% c("count_des", "count_asc")) {
      index_group <- which(outdata$group == sort_column)
      if (length(index_group) == 0) {
        message(paste(
          'If `sort_order` = "count_des" or "count_asc", `sort_column` should be specified as an existing column name.',
          "The table is sorted by the first group column."
        ))
        index_group <- 1
      }
    }

    # Sort if there are more than 4 rows
    if (nrow(res) > 4) {
      # Divide head and body for sorting
      res_head <- res[1:4, ]
      res_body <- res[5:nrow(res), ]
      soc_name <- toupper(soc_name[5:length(soc_name)])
      soc_order <- ifelse(outdata$order %% 1000 == 0, 0, 1)[5:length(outdata$order)]

      if (sort_order == "count_des") {
        if (all(c("soc", "par") %in% outdata$components)) {
          res_body <- cbind(res_body, soc_name, soc_order)
          res_body <- res_body[order(res_body[[paste0("n_", index_group)]], decreasing = TRUE), ]
          res_body <- res_body[order(res_body$soc_name, res_body$soc_order), names(res_head)]
        } else {
          res_body <- res_body[order(res_body$name), ]
          res_body <- res_body[order(res_body[[paste0("n_", index_group)]], decreasing = TRUE), ]
        }
      } else if (sort_order == "count_asc") {
        if (all(c("soc", "par") %in% outdata$components)) {
          res_body <- cbind(res_body, soc_name, soc_order)
          res_body <- res_body[order(res_body$soc_name, res_body$soc_order, res_body[[paste0("n_", index_group)]]), names(res_head)]
        } else {
          res_body <- res_body[order(res_body$name, res_body[[paste0("n_", index_group)]]), ]
        }
      } else {
        if (all(c("soc", "par") %in% outdata$components)) {
          res_body <- cbind(res_body, soc_name, soc_order)
          res_body <- res_body[order(res_body$soc_name, res_body$soc_order, res_body$name), names(res_head)]
        } else {
          res_body <- res_body[order(res_body$name), ]
        }
      }
      res <- rbind(res_head, res_body)
    }
  }

  # Transfer to Mock
  if (mock) {
    n_mock <- min(20, nrow(res), na.rm = TRUE)
    res <- to_mock(res, n = nrow(res)) |> as.data.frame()
  }

  if (mock) {
    res <- res[1:n_mock, ]
    outdata$order <- outdata$order[1:n_mock]
  }

  outdata$tbl <- res
  outdata$extend_call <- c(outdata$extend_call, match.call())
  outdata$filter_method <- filter_method
  outdata$filter_criteria <- filter_criteria

  outdata
}
