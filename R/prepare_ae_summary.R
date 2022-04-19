#    Copyright (c) 2022 Merck Sharp & Dohme Corp. a subsidiary of Merck & Co., Inc., Kenilworth, NJ, USA.
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

#' Prepare datasets for AE specific analysis
#'
#' @inheritParams prepare_ae_specific
#' @param ... additional parameters transfer to `prepare_ae_specific`
#'
#'
#' @examples
#' meta <- meta_ae_dummy()
#' prepare_ae_summary(meta, "apat", "wk12", "any;rel;ser")
#' @export
prepare_ae_summary <- function(meta,
                               population,
                               observation,
                               parameter,
                               ...) {
  parameters <- unlist(strsplit(parameter, ";"))

  res <- lapply(parameters, function(x) {
    print(x)
    prepare_ae_specific(meta,
      population = population, observation = observation, parameter = x,
      components = NULL, ...
    )
  })

  n_pop <- res[[1]]$n_pop
  tbl_num <- do.call(rbind, lapply(res, function(x) x$n[x$order == 100, ]))

  pop_prop <- res[[1]]$prop[1, ]
  tbl_prop <- do.call(rbind, lapply(res, function(x) x$prop[x$order == 100, ]))

  pop_diff <- res[[1]]$diff[1, ]
  tbl_diff <- do.call(rbind, lapply(res, function(x) x$diff[x$order == 100, ]))

  pop_ci <- res[[1]]$ci[1, ]
  tbl_ci <- do.call(rbind, lapply(res, function(x) x$ci[x$order == 100, ]))

  pop_p <- res[[1]]$p[1, ]
  tbl_p <- do.call(rbind, lapply(res, function(x) x$p[x$order == 100, ]))

  pop_name <- res[[1]]$name[1]
  name <- unlist(lapply(parameters, function(x) collect_adam_mapping(meta, x)$summ_row))

  outdata(meta, population, observation, parameter,
    n = rbind(n_pop, tbl_num),
    order = c(1, 1:nrow(tbl_num) * 100),
    group = res[[1]]$group,
    reference_group = res[[1]]$reference_group,
    prop = rbind(pop_prop, tbl_prop),
    diff = rbind(pop_diff, tbl_diff),
    n_pop = n_pop,
    name = c(pop_name, name)
  )
}

#' Format AE summary analysis
#'
#' @inheritParams format_ae_specific
#' @export
format_ae_summary <- format_ae_specific
