## Question 2 ###
# part b: --------------------------------
n = 1000
p = 100
mc_rep = 1000

Sigma = diag(array(1, p))
R = chol(Sigma)

X = rnorm(n*p*mc_rep, 0, 1)
dim(X) = c(n, p, mc_rep)
X_cor = array(0, c(n, p, mc_rep))  #create empty matrix (n*p*mc_rep)
for(m in 1:mc_rep){  
  X_cor[, , m] = X[, , m] %*% R   #transform to multivariate model
}

beta = c(rep(1, 10), rep(0, 90))
dim(beta) = c(p, 1)

# part a: --------------------------------
q2a = function(X_cor, beta, mc_rep, sigma=0.1){
  result_p_value = matrix(0, p, mc_rep)   #create empty matrix (p*mc_rep)
  n = nrow(X_cor)
  p = ncol(X_cor)
  for(m in 1:mc_rep){
    x_subset = X_cor[, , m]
    err = rnorm(n, 0, sigma)
    dim(err) = c(n, 1)
    Y = x_subset %*% beta + err
    
    #generate beta:--------------------
    QR = qr(x_subset)
    beta_hat = solve(qr.R(QR), t(qr.Q(QR))%*%Y)
    #Y hat:--------------------
    Y_hat = x_subset %*% beta_hat
    #estimate error variance: -----------------
    sigma2 = sum((Y - Y_hat)^2)/(n-p)
    #estimate vairance of beta: ---------------------
    beta_var = sigma2*chol2inv(qr.R(QR))
    SE = sqrt(diag(beta_var))
    
    #compute z test
    z = beta_hat / SE
    p_value = 2*(1-pnorm(abs(z)))
    
    result_p_value[, m] = p_value
  }
  return(result_p_value)
}


result_p_value = q2a(X_cor, beta, mc_rep, sigma=0.1)


# part c: --------------------------------

evaluate = function(result_p_value){
  alpha = 0.05
  test_result = ifelse(result_p_value < alpha, 1,0)  #reject null, label 1
  type_error = array(0, mc_rep)
  fwer_temp = array(0, mc_rep)
  fdr_temp = array(0, mc_rep)
  spe_temp = array(0, mc_rep)
  sen_temp = array(0, mc_rep)
  
  for (m in 1:mc_rep){
    #FWER
    fwer_temp[m] = sum(test_result[11:100, m])
    if(fwer_temp[m]>0){
      type_error[m] = 1
    }
    #fdr
    fdr_temp[m] = sum(test_result[11:100, m])/sum(test_result[,m])
    #specificity
    spe_temp[m] = sum({test_result[11:100, m]}==0)/90
    #sensitivity
    sen_temp[m] = sum(test_result[1:10, m])/10
  }
  
  fwer = mean(type_error)
  fdr = mean(fdr_temp)
  sensitivity = mean(sen_temp)
  specificity = mean(spe_temp)
  
  print(c(fwer, fdr, sensitivity, specificity))
  
}

uncorrected = evaluate(result_p_value)

# part d: ----------------------------------------------------
## bonferroni ##
adjust_p_bon = matrix(0, 100, mc_rep)
for (m in 1:mc_rep){
  p = result_p_value[, m]
  p_adjust = p.adjust(p, method = "bonferroni", n = length(p))
  adjust_p_bon[, m] = p_adjust
}

Bonferroni = evaluate(adjust_p_bon)

## Holm ##
adjust_p_holm = matrix(0, 100, mc_rep)
for (m in 1:mc_rep){
  p = result_p_value[, m]
  p_adjust = p.adjust(p, method = "holm", n = length(p))
  adjust_p_holm[, m] = p_adjust
}

Holm = evaluate(adjust_p_holm)

## BH ##
adjust_p_bh = matrix(0, 100, mc_rep)
for (m in 1:mc_rep){
  p = result_p_value[, m]
  p_adjust = p.adjust(p, method = "BH", n = length(p))
  adjust_p_bh[, m] = p_adjust
}

BH = evaluate(adjust_p_bh)


## BY ##
adjust_p_by = matrix(0, 100, mc_rep)
for (m in 1:mc_rep){
  p = result_p_value[, m]
  p_adjust = p.adjust(p, method = "BY", n = length(p))
  adjust_p_by[, m] = p_adjust
}

BY = evaluate(adjust_p_by)

# part e: ----------------------------------------
result = rbind(uncorrected, Bonferroni, Holm, BH, BY)
colnames(result) = c('FWER', 'FDR', 'Sensitivity', 'Specificity')

