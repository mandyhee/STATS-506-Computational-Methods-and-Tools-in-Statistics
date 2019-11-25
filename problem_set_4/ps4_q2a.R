library(dplyr)
library(tidyr)
library(data.table)
library(parallel)

# question 2
r = 0.1
p = 100
beta = c( rep(.1, floor(r*p)), rep(0, p - floor(r*p)) ) 
dim(beta) = c(p, 1)

rho = seq(-0.75, 0.75, 0.25)

func = function(rho, sigma = 1, p = 100, n=1000){
  
  # sim_beta: calculate p-value
  sim_beta = function(X, beta, sigma = 1, mc_rep){

    QR = qr( crossprod(X) )
    QX = X %*% qr.Q(QR) 
    XtXinv = solve( qr.R(QR), t( qr.Q(QR) ))
    n = nrow(X)
    p = ncol(X)
    Y = as.numeric(X %*% beta) + rnorm(n*mc_rep)
    dim(Y) = c(n, mc_rep)
    b = solve(qr.R(QR), crossprod( QX, Y ) )
    s_sq = colSums( {Y - as.numeric(X %*% b)}^2 / {n - p})
    
    v = sqrt( diag(XtXinv) * rep(s_sq, each = p) )
    
    P = matrix( 2*pt( abs( b / v ), df = {n-p}, lower.tail = FALSE ), p, mc_rep ) 
    return(P)
  }
  
  # evalute: calculate FWER, FDR, sensitivity, specificity (metric)
  evaluate = function(P, tp_ind = 1:10, alpha = .05){
    P = P < alpha
    p = nrow(P)
    n = ncol(P)
      
    TP = colSums(P[tp_ind, ])
    FP = colSums(P[-tp_ind,])
    TN = colSums(!P[-tp_ind,])
    FN = colSums(!P[tp_ind,])
      
    P = FP + TP
    fdr = ifelse(P > 0, FP  / {FP + TP}, 0)
    fwer = mean( FP > 0 )
    sens = TP / {TP + FN}
    spec = TN / {FP + TN}
      
    test = list( fwer = fwer, fwer_se = sqrt(fwer*{1-fwer} / n), 
          fdr =  mean(fdr), fdr_se = sd(fdr) / sqrt(n),
          sens = mean(sens), sens_se = sd(sens) / sqrt(n),
          spec = mean(spec), spec_se = sd(spec) / sqrt(n)
    )
    
    return(test)
  }
  
  
  Sigma = matrix(NA, nrow = 100, ncol = 100)
  Sigma = diag(p)
  for (i in 1:10){
    for (j in 1:10){
      if (i != j){
        Sigma[i, j] = 0.1*0.1*rho
      }
    }
  }
  R = chol(Sigma)
  X = matrix( rnorm(n*p), n, p) %*%  R
  
  #call out sim_beta
  P = sim_beta(X, beta, sigma = 1, mc_rep = 10000)
  # method: 'holm', 'bonferroni', 'BH', 'BY'
  # apply method in evalute
  all0 = lapply(c('holm', 'bonferroni', 'BH', 'BY'), function(x){
    evaluate(apply(P, 2, p.adjust, method = x), tp_ind = 1:10)
  })
  
  # generate result table
  all = rbindlist(all0)
  
  method = c('holm', 'bonferroni', 'BH', 'BY')
  table = cbind(method, all)
  
  table1 = dplyr::select(table, method, fwer, fdr, sens, spec)
  table2 = tidyr::gather(table1, metric, est, fwer:spec)
  table3 = table %>% 
    dplyr::select(method, fwer_se, fdr_se, sens_se, spec_se) %>% 
    dplyr::rename(fwer = fwer_se, fdr = fdr_se, sens = sens_se, spec = spec_se)
  table4 = tidyr::gather(table3, metric, se, fwer:spec)
  result = merge(table2, table4, by.x = c('method', 'metric'), by.y = c('method', 'metric'))
  result = cbind(rho = rho, sigma = sigma, result)
  return(result)
}

a = NULL
for (i in rho){
  a = rbind(a, func(i))
}

x = mclapply(1:7, function(rho_index){func(rho[rho_index])})
q2a = do.call("rbind", x)



