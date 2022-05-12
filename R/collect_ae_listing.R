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

#' Add inference information for AE listing analysis
#'
#' @param outdata a `outdata` object created by `prepare_ae_specific`
#' @param display a vector with name of variable used to display on AE listing.
#'
#' @import metalite
#' @examples
#' library(metalite)
#' library(metalite.ae)
#' meta <- meta_ae_dummy()
#' 
#' lapply(prepare_ae_specific(meta, "apat", "wk12", "rel") |> 
#' collect_ae_listing(), head, 10)
#' 
#' @export

collect_ae_listing <- function(outdata,
                               display = c('SEX', 'RACE', 'AGE', "ASTDY","AESEV","AESER", 
                                           "AEREL","AEACN","AEOUT", 'SITEID','ADURN', 'ADURU')) {
  res <- outdata
  
  obs_group <- collect_adam_mapping(res$meta, res$observation)$group
  obs_id <- collect_adam_mapping(res$meta, res$observation)$id
  par_var <- collect_adam_mapping(res$meta, res$parameter)$var
  
  res$meta$data_observation <- apply(res$meta$data_observation,2,toupper)
  
  obs <- collect_observation_record(res$meta, res$population, res$observation, res$parameter,
                                    var = c(par_var, obs_id, obs_group, display)
  )
  
  res$ae_listing <- obs[,!(names(obs) == "SAFFL")]
  res
}
