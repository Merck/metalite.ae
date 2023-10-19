extend_ae_summary_eaer <- function(outdata,
                                   duration_var = "TRTDUR",
                                   adj_unit = c("year", "month", "week", "day"),
                                   ...) {
  time_unit <- list("year" = 365.24, "month" = 30.4367, "week" = 7, "day" = 1)
  adj_unit <- match.arg(adj_unit)
  exp_factor <- 100 * time_unit[[adj_unit]]

  # prep
  pop_var <- collect_adam_mapping(outdata$meta, outdata$population)$var
  pop <- collect_population_record(outdata$meta, outdata$population, var = c(pop_var, duration_var))
  pop_group <- collect_adam_mapping(outdata$meta, outdata$population)$group

  # den: Total exposure in person-year/month/week/day
  total_exposure <- tapply(pop[[duration_var]], pop[[pop_group]], FUN = sum)


  parameters <- unlist(strsplit(outdata$parameter, ";"))

  res <- lapply(parameters, function(x) {
    message(x)
    den <- total_exposure
    num <- f_nae(x, outdata$meta, outdata$population)
    ans <- num * exp_factor / den
  })

  adj_rate_table <- do.call(rbind, res)

  metalite::outdata(meta, population, observation, parameter,
    n = rbind(n_pop, tbl_num),
    order = c(1, seq_len(nrow(tbl_num)) * 100),
    group = res[[1]]$group,
    reference_group = res[[1]]$reference_group,
    prop = rbind(pop_prop, tbl_prop),
    diff = rbind(pop_diff, tbl_diff),
    n_pop = n_pop,
    name = c(pop_name, name),
    prepare_call = match.call(),
    eaer = eaer
  )
}


f_nae <- function(x, meta, observation) {
  # prep
  obs_group <- collect_adam_mapping(meta, observation)$group

  # start computation
  if (x == "any") {
    data <- meta$data_observation
    num <- sapply(split(data, data[[obs_group]]), function(x) length(x[[obs_group]]))
  } else {
    data <- meta$data_observation
    expr <- collect_adam_mapping(meta, x)$subset
    data <- data %>% subset(eval(expr))
    num <- sapply(split(data, data[[obs_group]]), function(x) length(x[[obs_group]]))
  }
  return(num)
}
