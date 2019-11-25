#{problem set question 3}
library(dplyr)
library(tidyr)
library(margins)
library(knitr)
demo = read.csv("demo.csv")
ohx = read.csv("ohx.csv")
#{part a}: merge demo and ohx by seqn
merge <- merge(demo, ohx, by.demo = seqn)

#{part b}: recode ohx04htc to outcome (2 levels), fit logit
merge = merge %>%
  filter(ohx04htc != 9) %>%
  filter(!is.na(ohx04htc)) %>%
  mutate(outcome = factor(ohx04htc, levels = c(1,2,4,5), labels = c(0,1,1,1)))

model1 = glm(outcome ~ ridagemn, data = merge, family = binomial(link = 'logit'))
BIC(model1)   #BIC = 1533.407

#calculate representative age
age25 = round(((log(0.25/(1-0.25)) - coef(model1)[1])/coef(model1)[2]), digits = 0)    #age25 = 104
age50 = round(((log(0.5/(1-0.5)) - coef(model1)[1])/coef(model1)[2]), digits = 0)    #age50 = 119
age75 = round(((log(0.75/(1-0.75)) - coef(model1)[1])/coef(model1)[2]), digits = 0)    #age75 = 135 
age25_yr = floor(age25/12)   #8
age75_yr = ceiling(age75/12) #12
age = c(age25_yr, age75_yr)

#{part c}: add covariates
model2 = glm(outcome ~ ridagemn + riagendr, data = merge, family = binomial(link = 'logit'))
BIC(model2)   #BIC = 1542.055 -->remove gender

#recode race to three level and fit model
merge = merge %>%
  mutate(race = factor(ridreth1, levels = c(1,2,3,4,5), labels = c(1,2,0,3,2))) %>%
  mutate(race_1 = factor(race, levels = c(0,1,2,3), labels = c(0,1,0,0))) %>%
  mutate(race_2 = factor(race, levels = c(0,1,2,3), labels = c(0,0,1,0))) %>%
  mutate(race_3 = factor(race, levels = c(0,1,2,3), labels = c(0,0,0,1))) 
  
model3 = glm(outcome ~ ridagemn + race_1, data = merge, family = binomial(link = 'logit'))
BIC(model3)   #BIC = 1542.285 --> remove race_1

model4 = glm(outcome ~ ridagemn + race_2, data = merge, family = binomial(link = 'logit'))
BIC(model4)  #BIC = 1541.932 --> remove race_2

model5 = glm(outcome ~ ridagemn + race_3, data = merge, family = binomial(link = 'logit'))
BIC(model5)   #BIC = 1529.281 --> keep race_3

#add poverty income ratio to the model (final model)
model6 = glm(outcome ~ ridagemn + race_3 + indfmpir, data = merge, family = binomial(link = 'logit'))
BIC(model6)#BIC = 1462.895 --> keep pir

#{part d}: marginal means
#adjusted prediction at representative age 
pr_age25 = 1/(1+exp(-(coef(model1)[1]+(coef(model1)[2]*age25))))
pr_age75 = 1/(1+exp(-(coef(model1)[1]+(coef(model1)[2]*age75))))


#marginal effects at the mean (categorical variable: race_3, mean of pir = 2.46)
pr_age25_race1 = 1/(1+exp(-(coef(model6)[1] + coef(model6)[2]*age25 + coef(model6)[3]*1 + coef(model6)[4]*2.46)))
pr_age25_race0 = 1/(1+exp(-(coef(model6)[1] + coef(model6)[2]*age25 + coef(model6)[3]*0 + coef(model6)[4]*2.46)))
mse_25 = pr_age25_race1 - pr_age25_race0

pr_age75_race1 = 1/(1+exp(-(coef(model6)[1] + coef(model6)[2]*age75 + coef(model6)[3]*1 + coef(model6)[4]*2.46)))
pr_age75_race0 = 1/(1+exp(-(coef(model6)[1] + coef(model6)[2]*age75 + coef(model6)[3]*0 + coef(model6)[4]*2.46)))
mse_75 = pr_age75_race1 - pr_age75_race0

#average marginal effect 
ame = summary(margins(model6, at = list(ridagemn = c(104, 136)), variables = c("race_3")))








