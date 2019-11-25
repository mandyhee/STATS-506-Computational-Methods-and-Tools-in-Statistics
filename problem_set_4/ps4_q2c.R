# libraries
library(parallel)
library(future)
source("ps4_q2a.R")
source("ps4_q2b.R")

# default arguments
args_list = list(
  sigma=1,
  mc_rep=10000,
  n_cores=4
)
args = list("sigma=1", "mc_rep", "n_cores=4")

## get parameters from command line
args = commandArgs(trailingOnly = TRUE)
print(args)

# functions for finding named arguments
args_to_list = function(args){
  ind = grep('=', args)  
  args_list = sapply(args[ind], strsplit, '=')
  names(args_list) = sapply(args_list, function(x) x[1])
  
  args_list = lapply(args_list, function(x) as.numeric(x[2]))
  args_list
}

# get named arguments
args_list_in = args_to_list(args)

# update non default arguments
ignored = c()
for ( arg in names(args_list_in) ) {
  # Check for unknown argument
  if ( is.null(args_list[[arg]]) ) {
    ignored = c(ignored, arg)
  } else{
    # update if known
    args_list[[arg]] = args_list_in[[arg]]
  }
}

# set values
sigma = args_list$sigma
mc_rep = args_list$mc_rep
n_cores = args_list$n_cores

# -----------------------------------------------------------------------------
# Do the computations
plan(multiprocess)
rho = seq(-0.75, 0.75, 0.25)

result = foreach(i=sigma, .packages = c('data.table', 'dplyr')) %:%
  foreach(j = rho, .packages =  c('data.table', 'dplyr')) %dopar% {
    func(rho = j , sigma = i)
  }

q2c = NULL
for(i in 1:length(q2c)){
  q2c = future(rbind(q2c, do.call("rbind", result[[i]])))
}




