setup_sim_matrix = function(mainpath, net, beta, lower_th) {
  path = sprintf("%sSimilarities/%s/simil_list_beta_",mainpath, net)
  sim = read.csv(paste0(path, beta, ".csv"))
  logpara = "log"
  rownames(sim) = sim$X
  sim = sim[,-c(1)]
  colnames(sim) = rownames(sim)
  
  simpos = sim
  simpos[is.na(simpos)]=NA
  simvalues = c(as.matrix(simpos))
  simvalues = simvalues[!is.na(simvalues)]
  simvalues = simvalues[simvalues!=-1]
  
  sim[sim<=0] = NA
  if (logpara == "log") {
    sim = log(sim)
  }
  
  ###Null model
  simpos = sim
  simpos[is.na(simpos)]=NA
  simvalues = c(as.matrix(simpos))
  simvalues = simvalues[!is.na(simvalues)]
  
  qua = quantile(simvalues, lower_th)
  
  ### Lower threshold
  sim[(as.matrix(sim)<qua)] = NA
  
  #Normalize
  simmin = min(sim, na.rm = T)
  simmax = max(sim, na.rm = T)
  
  sim[!is.na(sim)] = (sim[!is.na(sim)]-simmin)/(simmax - simmin)
  return(sim)
}