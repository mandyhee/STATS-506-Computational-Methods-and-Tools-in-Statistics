library(doParallel)
library(parallel)
library(foreach)
source("ps4_q2a.R")


ncores = 4
cl = makeCluster(ncores)
registerDoParallel(cl)

# do job

rho = seq(-0.75, 0.75, 0.25)
sigma = c(0.25, 0.5, 1)

result_q4b = foreach(i=sigma, .packages = c('data.table', 'dplyr')) %:%
  foreach(j = rho, .packages =  c('data.table', 'dplyr')) %dopar% {
  p2b = func(rho = j , sigma = i)
}


q2b = NULL
for(i in 1:length(result_q4b)){
  q2b = rbind(q2b, do.call("rbind", result_q4b[[i]]))
}


save(q2b, file = "results_q4b.RData")





