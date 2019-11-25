/*problem set 2 question 1*/

//import file
import delimited recs2015_public_v3.csv
save recs 
use recs


//calculate sample estimates using nweight; kwh: 10720.36, cufeetng: 355.2492, gallonlp: 33.42942, gallonfo: 28.60146
use recs
keep doeid kwh cufeetng gallonlp gallonfo nweight
save recs_1

egen total_nweight = sum(nweight) //sum of nweight (denominator)

gen kwh_m = kwh*nweight / total_nweight //kwh
egen kwh_full = sum(kwh_m)

gen cufeetng_m = cufeetng*nweight / total_nweight //cufeetng
egen cufeetng_full = sum(cufeetng_m)

gen gallonlp_m = gallonlp*nweight / total_nweight //gallonlp
egen gallonlp_full = sum(gallonlp_m)

gen gallonfo_m = gallonfo*nweight / total_nweight //gallonfo
egen gallonfo_full = sum(gallonfo_m)

save recs_2

//------------------------------
*create replicate estimates: kwh
*-------------------------------//
use recs
keep doeid brrwt1-brrwt96 kwh 
save recs_kwh

foreach var of varlist brrwt1-brrwt96 {		//calculate kwh*brrwt(i)
  generate kwh_`var' = `var' * kwh
}

egen total_brrwt = sum(brrwt1)	//calculate total replicate brrwt (same accross brrwt1-brrwt2) (denominator)
 
foreach var of varlist kwh_brrwt1-kwh_brrwt96 { //sum of kwh*brrwt (numerator)
egen m_`var' = sum(`var') 
}

foreach var of varlist m_kwh_brrwt1-m_kwh_brrwt96 { //generate theta1, theta2... theta(r)
generate r_`var' = `var' / total_brrwt
}
save recs_kwh1
keep r_m_kwh_brrwt1 - r_m_kwh_brrwt96
duplicates drop
//calcuate variance: kwh (sample estimates:10720.36, rse: 1.06)
gen kwh_mean = 10720.36
reshape long r_m_kwh_brrwt, i(kwh_mean)
rename r_m_kwh_brrwt kwh_wm
gen _sq = (kwh_wm - kwh_mean)^2
egen _sum = sum(_sq)
gen kwh_var = 0.04*_sum
gen kwh_rse = (sqrt(kwh_var) / kwh_mean)*100
save kwh_m_rse



//-----------------------------------
*create replicate estimates: cufeetng
*-------------------------------------//
use recs
keep doeid brrwt1-brrwt96 cufeetng
save recs_cufeetng

foreach var of varlist brrwt1-brrwt96 {		//calculate cufeetng*brrwt(i)
  generate cufeetng_`var' = `var' * cufeetng
}

egen total_brrwt = sum(brrwt1)	//calculate total replicate brrwt (same accross brrwt1-brrwt2) (denominator)
 
foreach var of varlist cufeetng_brrwt1-cufeetng_brrwt96 { //sum of cufeetng*brrwt (numerator)
egen m_`var' = sum(`var') 
}

foreach var of varlist m_cufeetng_brrwt1-m_cufeetng_brrwt96 { //generate theta1, theta2... theta(r)
generate r_`var' = `var' / total_brrwt
}
save recs_cufeetng1
keep r_m_cufeetng_brrwt1 - r_m_cufeetng_brrwt96
duplicates drop
//calcuate variance: cufeetng (sample estimates:355.25, rse: 11.15)
gen cufeetng_mean = 355.2492
reshape long r_m_cufeetng_brrwt, i(cufeetng_mean)
rename r_m_cufeetng_brrwt cufeetng_wm
gen _sq = (cufeetng_wm - cufeetng_mean)^2
egen _sum = sum(_sq)
gen cufeetng_var = 0.04*_sum
gen cufeetng_rse = (sqrt(cufeetng_var) / cufeetng_mean)*100
save cufeetng_m_rse


//-----------------------------------
*create replicate estimates: gallonlp
*-------------------------------------//
use recs
keep doeid brrwt1-brrwt96 gallonlp
save recs_gallonlp

foreach var of varlist brrwt1-brrwt96 {		//calculate gallonlp*brrwt(i)
  generate gallonlp_`var' = `var'*gallonlp
}

egen total_brrwt = sum(brrwt1)	//calculate total replicate brrwt (same accross brrwt1-brrwt2) (denominator)
 
foreach var of varlist gallonlp_brrwt1-gallonlp_brrwt96 { //sum of gallonlp*brrwt (numerator)
egen m_`var' = sum(`var') 
}

foreach var of varlist m_gallonlp_brrwt1-m_gallonlp_brrwt96 { //generate theta1, theta2... theta(r)
generate r_`var' = `var' / total_brrwt
}
save recs_gallonlp1
keep r_m_gallonlp_brrwt1 - r_m_gallonlp_brrwt96
duplicates drop
//calcuate variance: gallonlp (sample estimates:33.42942, rse: 12.20)
gen gallonlp_mean = 33.42942
reshape long r_m_gallonlp_brrwt, i(gallonlp_mean)
rename r_m_gallonlp_brrwt gallonlp_wm
gen _sq = (gallonlp_wm - gallonlp_mean)^2
egen _sum = sum(_sq)
gen gallonlp_var = 0.04*_sum
gen gallonlp_rse = (sqrt(gallonlp_var) / gallonlp_mean)*100
save gallonlp_m_rse



//-----------------------------------
*create replicate estimates: gallonfo
*-------------------------------------//
use recs
keep doeid brrwt1-brrwt96 gallonfo
save recs_gallonfo

foreach var of varlist brrwt1-brrwt96 {		//calculate gallonfo*brrwt(i)
  generate gallonfo_`var' = `var'*gallonfo
}

egen total_brrwt = sum(brrwt1)	//calculate total replicate brrwt (same accross brrwt1-brrwt2) (denominator)
 
foreach var of varlist gallonfo_brrwt1-gallonfo_brrwt96 { //sum of gallonlp*brrwt (numerator)
egen m_`var' = sum(`var') 
}

foreach var of varlist m_gallonfo_brrwt1-m_gallonfo_brrwt96 { //generate theta1, theta2... theta(r)
generate r_`var' = `var' / total_brrwt
}
save recs_gallonfo1
keep r_m_gallonfo_brrwt1 - r_m_gallonfo_brrwt96
duplicates drop
//calcuate variance: gallonfo (sample estimates: 28.60146, rse: 8.37)
gen gallonfo_mean = 28.60146
reshape long r_m_gallonfo_brrwt, i(gallonfo_mean)
rename r_m_gallonfo_brrwt gallonfo_wm
gen _sq = (gallonfo_wm - gallonfo_mean)^2
egen _sum = sum(_sq)
gen gallonfo_var = 0.04*_sum
gen gallonfo_rse = (sqrt(gallonfo_var) / gallonfo_mean)*100
save gallonfo_m_rse

//------------------------------------------
*data management: create recs2015_usage.csv
*append estimate, variance, rse
*---------------------------------------------//
//kwh
use kwh_m_rse
keep kwh_mean kwh_var kwh_rse
duplicates drop
gen var = "kwh"
rename kwh_mean estimate
rename kwh_var variance
rename kwh_rse replicate_standard_error
order var estimate variance replicate_standard_error
save kwh

//cufeetng
use cufeetng_m_rse
keep cufeetng_mean cufeetng_var cufeetng_rse
duplicates drop
gen var = "kwh"
rename cufeetng_mean estimate
rename cufeetng_var variance
rename cufeetng_rse replicate_standard_error
order var estimate variance replicate_standard_error
save cufeetng

//gallonlp
use gallonlp_m_rse
keep gallonlp_mean gallonlp_var gallonlp_rse
duplicates drop
gen var = "gallonlp"
rename gallonlp_mean estimate
rename gallonlp_var variance
rename gallonlp_rse replicate_standard_error
order var estimate variance replicate_standard_error
save gallonlp

//gallonfo
use gallonfo_m_rse
keep gallonfo_mean gallonfo_var gallonfo_rse
duplicates drop
gen var = "gallonfo"
rename gallonfo_mean estimate
rename gallonfo_var variance
rename gallonfo_rse replicate_standard_error
order var estimate variance replicate_standard_error
save gallonfo

//append four dataset
use kwh
append using cufeetng
append using gallonlp
append using gallonfo
rename var variable
save q1
export delimited recs2015_usage




