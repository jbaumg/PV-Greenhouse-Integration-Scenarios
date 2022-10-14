

generate_scenarios<-function(site, yt, controllable_demands=NULL, gridcost, feed_in, pvcost, storagecost, interest_rate, pv_runtime, storage_runtime, efficiency_storage, name, max_power_to_grid){
  
  print(paste0("running scenario ",name))
  
  feed_in_tariff <- feed_in
  
  namesc <- paste0("scenario",name)
  
  site <- site
  
  if(site==0){inp<-demanddf}
  if(site==1){inp<-demanddf2}
  
  if(site==0){pv<-pv}
  if(site==1){pv<-pv}
  
  if(length(controllable_demands)==0){shift<-"noshift"}
  if(length(controllable_demands)==1){shift<-"midshift"}
  if(length(controllable_demands)>1){shift<-"highshift"}
  
  shift <- shift
  
  inp <- inp%>%filter(year(date) > (yt - 1) & year(date) < (yt + 1))
  
  pv <- pv%>%filter(year(date) > (yt - 1) & year(date) < (yt + 1))
  
  pv <- pv$pred
  
  periods <- NROW(inp)
  
  timesteps <- periods
  
  if(!is.null(controllable_demands)){
  
  demnames <- sapply(1:length(controllable_demands), function(i, inp) {
  
    dtx <- as.vector(unlist(inp[paste0("pred.",controllable_demands[i])]))%>%
      tibble() %>%
      mutate(dt=paste0("p",controllable_demands[i])) %>%
      mutate(d=paste("d",rep(1:(periods/24),each = 24),sep="")) %>%
      group_by(dt,d) %>%
      summarize(value=sum(.))%>%
      select(value,dt,d)
    
    assign(paste0("controllable_demand", controllable_demands[i]), dtx, envir = globalenv())
  
    name<-paste0("controllable_demand", controllable_demands[i])
    return(name)
    
  }, inp = inp)
  
  demnames2 <- sapply(1:length(controllable_demands), function(i, inp) {
    
    dtx <- as.vector(unlist(inp[paste0("pred.",controllable_demands[i])]))%>%
      tibble() %>%
      mutate(dt=paste0("p",controllable_demands[i])) %>%
      mutate(d=paste("d",1:periods,sep=""))
    
    assign(paste0("controllable_demandh", controllable_demands[i]), dtx, envir = globalenv())
    
    name<-paste0("controllable_demandh", controllable_demands[i])
    return(name)
    
  }, inp = inp)
  
  cdnames <- sapply(1:length(controllable_demands), function(i) {
    
    t_in_d <- tibble(dt=c(paste0("p",controllable_demands[i])),
                    d=c(paste("d",rep(1:(periods/24),each = 24),sep="")),
                    t=c(paste("t",seq(1:periods),sep="")),
                    value=c(rep(1,periods)))
  
    assign(paste0("t_in_d_p", controllable_demands[i]), t_in_d, envir = globalenv())
  
    name<-paste0("t_in_d_p", controllable_demands[i])
    return(name)
  })
  
  t_in_d <- bind_rows(mget(cdnames, envir = .GlobalEnv))
  
  controllable_demand_full <- bind_rows(mget(demnames, envir = .GlobalEnv))
  
  controllable_demand_full2 <- bind_rows(mget(demnames2, envir = .GlobalEnv))
  
  }else{
    
    controllable_demand_full <- as.vector(rep(0,NROW(inp)))%>%
    tibble() %>%
    mutate(dt=paste0("p",1)) %>%
    mutate(d=paste("d",rep(1:(periods/24),each = 24),sep="")) %>%
    group_by(dt,d) %>%
    summarize(value=sum(.))%>%
    select(value,dt,d)
    
    controllable_demand_full2 <- as.vector(rep(0,NROW(inp)))%>%
      tibble() %>%
      mutate(dt=paste0("p",1)) %>%
      mutate(d=paste0("d",1:periods)) 
    
    t_in_d <- tibble(dt=c(paste0("p",1)),
                    d=c(paste("d",rep(1:(periods/24),each = 24),sep="")),
                    t=c(paste("t",seq(1:periods),sep="")),
                    value=c(rep(0,periods)))
    
  }
  
  controllable_demand_full$d <- factor(controllable_demand_full$d,levels=unique(controllable_demand_full$d))
  
  names(controllable_demand_full) <- c("value", "dt", "d")
  
  controllable_demand_full2$d <- factor(controllable_demand_full2$d,levels=unique(controllable_demand_full2$d))
  
  names(controllable_demand_full2) <- c("value", "dt", "d")
  
  controllable_demand_sum <- controllable_demand_full2 %>% group_by(d) %>% summarize(cdem_sum = sum(value))
  
  cdem_max <- controllable_demand_full %>% group_by(dt) %>% summarize(cdem_max = max(value))
  
  maximum_power_controllable_demand <- cdem_max$cdem_max
  
  controllable_demand_full$d <- as.character(controllable_demand_full$d)
  
  controllable_demand_in <- controllable_demand_full %>%
  dplyr::select(dt, d, value)
  
  demand <- inp$preds - controllable_demand_sum$cdem_sum
  
  pv_invest_annualized <- annualize(pvcost,
                                  interest_rate,
                                  pv_runtime,
                                  8760)
  
  storage_invest_annualized <- annualize(storagecost,
                                       interest_rate,
                                       storage_runtime,
                                       8760)
  
  create_input_data(timesteps = timesteps,
                  demand_in = demand,
                  pv_gen = pv,
                  controllable_demand_in = controllable_demand_in,
                  pv_invest_annualized = pv_invest_annualized,
                  storage_invest_annualized = storage_invest_annualized,
                  gridcost = gridcost,
                  feed_in_tariff = feed_in_tariff,
                  efficiency_storage = efficiency_storage,
                  maximum_power_controllable_demand = maximum_power_controllable_demand,
                  t_in_d = t_in_d,
                  namesc = namesc,
                  site = site,
                  shift = shift,
        	  max_power_to_grid = max_power_to_grid)
  
  print(system.time(gams(paste0("path/pvls.gms --site=scenarioout",site," --shift=",shift," --namesc=",namesc,""))))
  
  mygdx <- gdx('path/output.gdx')
  
  #### THESE 2 VALUES HAVE TO BE 1, otherwise there was a problem when solving!
  #### if here a statement is printed, something went wrong and results should not be used.
  if(mygdx["modelstat"] != 1){
    print("Caution: model was not solved properly, results should not be trusted.")
  }
  if(mygdx["solvestat"] != 1){
    print("Caution: model was not solved properly, results should not be trusted.")
  }

  print(paste("modelstat:",mygdx["modelstat"]))
  
  print(paste("solvestat:",mygdx["solvestat"]))
  
  installed_pv_capacity <- mygdx["x_pv"]
  
  print(paste("installed_pv_capacity:",installed_pv_capacity,"Wp at cost:",pvcost,"EUR/Wp"))
  
  installed_storage_capacity <- mygdx["x_storage"]
  
  print(paste("installed_storage_capacity:",installed_storage_capacity,"W at cost:",storagecost, "EUR/Wh"))
  
  scenarioIO <- data.frame(site, yt, gridcost, feed_in, pvcost, storagecost, interest_rate, pv_runtime, storage_runtime, efficiency_storage, name, mygdx["modelstat"], mygdx["solvestat"], installed_pv_capacity, installed_storage_capacity, mygdx["x_cost"], sum(mygdx["x_control_demand"]$value) )
  names(scenarioIO)<-c("site", "year", "gridcost", "feed_in", "pvcost", "storagecost", "interest_rate", "pv_runtime", "storage_runtime", "efficiency_storage", "name", "modelstat", "solvestat", "installed_pv_capacity", "installed_storage_capacity", "x_cost", "sum_demand_dispatch")
  
  return(scenarioIO)
  
  }