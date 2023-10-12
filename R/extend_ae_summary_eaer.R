
f_nae=function(x,meta,observation){
  # prep
  obs_group <- collect_adam_mapping(meta, observation)$group

  # start computation
  if(x=='any'){
    data <- meta$data_observation
    num <- sapply(split(data, data[[obs_group]]), function(x) length(x[[obs_group]]))
  }else{
    data <- meta$data_observation
    expr <- collect_adam_mapping(meta, x)$subset;
    data <- data %>% subset(eval(expr))
    num <- sapply(split(data, data[[obs_group]]), function(x) length(x[[obs_group]]))
  }
  return(num)
}

f_eaer = function(meta,population,observation){
  # prep
  pop_var =   collect_adam_mapping(meta, population)$var
  duration_var = "TRTDUR"
  pop <- collect_population_record(meta, population, var = c(pop_var, duration_var))
  pop_group <- collect_adam_mapping(meta, population)$group

  # den: Total exposure in person-year/month/week/day
  total_exposure <- tapply(pop$TRTDUR, pop[[pop_group]], FUN = sum);total_exposure
  # 'any'
  den <- total_exposure; num <- f_nae("any",meta,observation);ans <- num * exp_factor / den; row1=t(ans);
  # "rel"
  den <- total_exposure;num <- f_nae("rel",meta,observation);ans <- num * exp_factor / den ; row2=t(ans);
  # "ser"
  den <- total_exposure; num <- f_nae("ser",meta,observation);ans <- num * exp_factor / den ;row3=t(ans);
  adj_rate_table = rbind.data.frame(row1,row2,row3)
  rownames(adj_rate_table) = c("any","rel","ser")

  return(adj_rate_table)
}

