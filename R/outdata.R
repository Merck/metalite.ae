#' Evaluate a call using outdata
#'
#' @param outdata a outdata object.
#' @param call a function call that require `outdata` object as an input.
#'
#' @noRd
outdata_eval_extend_call <- function(outdata, call){

  call$outdata <- str2lang("outdata")
  eval(call)

}

#' Evaluate a call using outdata
#'
#' @param outdata a outdata object.
#' @param call a function call that require `outdata` object as an input.
#'
#' @noRd
outdata_eval_prepare_call <- function(outdata){

  call <- outdata$prepare_call
  call$meta <- str2lang("outdata$meta")

  population <- names(outdata$meta$population)

  res <- list()
  for(i in seq_along(population)){

    call$population <- population[i]

    outdata_subgroup <- eval(call)

    extend_call <- outdata$extend_call
    for(j in seq_along(extend_call)){
      outdata_subgroup <- outdata_eval_extend_call(outdata_subgroup, extend_call[[j]])
    }

    res[[i]] <- outdata_subgroup

  }

  res

}

#' Update outdata subgroup population
#'
#' @inheritParams extend_ae_specific_subgroup
#'
#' @noRd
outdata_population_subgroup <- function(
    outdata,
    subgroup){

  meta <- outdata$meta

  # define subgroup
  subgroup <- outdata$subgroup
  u_subgroup <- unique(meta$data_population[[subgroup]])

  pop_name <- names(meta$population)
  pop_subset <- unlist(lapply(pop_name, function(x) deparse(meta$population[[x]]$subset)))
  pop_subset <- ifelse(nchar(trimws(pop_subset)) == 0, "TRUE", pop_subset)

  subgroup_subset <- paste0(subgroup, '== "', u_subgroup, '"')


  new_subset <- outer(pop_subset, subgroup_subset, paste, sep = " & ")
  new_name <- outer(pop_name, u_subgroup, paste, sep = "-")

  for(i in 1:nrow(new_name)){
    for(j in 1:ncol(new_name)){
      x <- meta$population[[pop_name[i]]]
      x$name <- new_name[i,j]
      x$subset <- str2lang(new_subset[i,j])
      meta$population[[new_name[i,j]]] <- x

    }
  }

  meta$population <- meta$population[as.vector(new_name)]
  outdata$meta <- meta

  outdata

}