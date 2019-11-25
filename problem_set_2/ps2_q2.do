* problem set 2 question 2 
* read file
fdause DEMO_D.XPT
save demo
* export csv for R
export delimited demo.csv 
fdause OHX_D.XPT
save ohx
* export csv for R
export delimited ohx.csv 

*part a: merge demo and ohx by seqn*
use demo
merge 1:1 seqn using ohx
save _merge

* part b: logistic regression 

use _merge
drop if ohx04htc == 9   
drop if ohx04htc == .
recode ohx04htc (1 = 0) (2 4 5 =1), gen(outcome) 
* check if outcome successfully created
tab ohx04htc outcome 

quietly glm outcome ridagemn, family(binomial) link(logit)
estat ic   

* part c: add covariate 


quietly glm outcome ridagemn riagendr, family(binomial) link(logit)
estat ic 
* remove gender  

* create race categorical variables
recode ridreth1 (3 = 0) (1 = 1) (2 5 = 2) (4 = 3), gen(race) 
tab ridreth1 race	

recode race (1 = 1) (0 = 0) (2 = 0) (3 = 0), gen(race_1)
recode race (1 = 0) (0 = 0) (2 = 1) (3 = 0), gen(race_2)
recode race (1 = 0) (0 = 0) (2 = 0) (3 = 1), gen(race_3)

save merge1

* model fitting
quietly glm outcome ridagemn race_1, family(binomial) link(logit)
estat ic 
* remove race_1

quietly glm outcome ridagemn race_2, family(binomial) link(logit)
estat ic 
* remove race_2

quietly glm outcome ridagemn race_3, family(binomial) link(logit)
estat ic 
* retain race_3


quietly glm outcome ridagemn race_3 indfmpir, family(binomial) link(logit)
estat ic 
* retain pir
* final model


* part d margin
* 1. adjusted predictions
quietly logit outcome ridagemn, nolog
adjust ridagemn = 104, pr 
adjust ridagemn = 136, pr 

* 2. marginal effects at mean
quietly logit outcome ridagemn race_3 indfmpir, nolog
margins, dydx(race_3) at(ridagem = (104, 136)) atmeans vsquish

* 3. average marginal effect
margins, dydx(race_3) at(ridagem = (104, 136)) vsquish


* part e 
svyset sdmvpsu [pweight=wtmec2yr], strata(sdmvstra) vce(linearized)
svy: glm outcome ridagemn race_3 indfmpir, family(binomial) link(logit)

* create table: compart final model vs final model using survey weight
glm outcome ridagemn race_3 indfmpir, family(binomial) link(logit)
outreg2 using compare.doc, replace ctitle(final model) 

svy: glm outcome ridagemn race_3 indfmpir, family(binomial) link(logit)
outreg2 using compare.doc, append ctitle(final model using survey weights)
* difference: by looking at the table (compare.doc), there are around 0.01 difference between the coefficients in final model vs final model using survey weights 





