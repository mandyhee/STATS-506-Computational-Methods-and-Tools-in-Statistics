## Question 1 ##

library(data.table)
library(reshape2)

recs = read.csv("recs2015_public_v3.csv")
recs = data.table(recs)

#create replicate weight (long format): -------------------------------------------
brrwt = list()
for (i in 1:96){
  wt = paste("BRRWT", i, sep="")
  brrwt = append(brrwt, wt)
}
brrwt = unlist(brrwt)

weights = melt(recs, id.vars = "DOEID", measure.vars = brrwt, variable.name = "weight")

#division: -------------------------------------------
divisions = c(
  'New England',
  'Middle Atlantic',
  'East North Central',
  'West North Central',
  'South Atlantic',
  'East South Central',
  'West South Central',
  'Mountain North',
  'Mountain South',
  'Pacific'
)

recs0 = recs[, `:=`(division= factor(DIVISION, 1:10, divisions))]

#### part1 #####
#point estimate: -------------------------------------------
p_stucco = recs0[, .(DOEID, division, NWEIGHT, WALLTYPE), by = division
                ][order(division), .(p_stucco = sum(NWEIGHT*{WALLTYPE == 4}) / sum(NWEIGHT)), by = division]

#replicate weigth: -------------------------------------------
recs1 = recs0[ ,.(DOEID, division, NWEIGHT, WALLTYPE)]
setkey(recs1, DOEID)
setkey(weights, DOEID)
p_stucco_r = merge(recs1, weights, all.x = TRUE)

p_stucco_r = p_stucco_r[, .(r_stucco = sum(value*{WALLTYPE == 4}) / sum(value)), by = .(division, weight)]

#standard error: -------------------------------------------
setkey(p_stucco_r, division)
setkey(p_stucco, division)
p_stucco_r = merge(p_stucco_r, p_stucco, all.x = TRUE)

p_stucco = p_stucco_r[order(p_stucco), .(p_stucco = p_stucco[1], se_stucco = 2 * sqrt(mean({r_stucco - p_stucco}^2))), by = division
                                         ][, `:=`(lwr = pmax(p_stucco - qnorm(.975)*se_stucco, 0), upr = p_stucco + qnorm(.975)*se_stucco), by = division
                                           ][order(p_stucco), .(division, p_stucco = p_stucco*100, lwr = lwr*100, upr = upr*100)]

table1 = p_stucco[, .(division, point_estimate = sprintf('%4.1f%%(%4.1f, %4.1f)', p_stucco, lwr, upr))]

#largest = Mountain South; lowest: East South Central

#graph
plot1 = ggplot(p_stucco,  aes(x = division, y = p_stucco)) +
  geom_bar(stat="identity") +
  geom_errorbar( aes(ymin=lwr, ymax=upr), col='navy' ) + 
  xlab('Division') + 
  ylab('% of homes having STUCCO as major walltype') +
  coord_flip()

#### part 2 ####
recs2 = recs[, `:=`(urban = UATYP10 %in% c('U', 'C'), division = factor(DIVISION, 1:10, divisions))]
#average total kwh of electricity usage by division
#point estimate: -------------------------------------------
avg_kwh = recs2[, .(avg_kwh = sum(KWH*NWEIGHT)/sum(NWEIGHT)), by = division][order(avg_kwh), ]

#replicate weight: -------------------------------------------
recs2 = recs2[ ,.(DOEID, division, NWEIGHT, KWH)]
setkey(recs2, DOEID)
setkey(weights, DOEID)
avg_kwh_r = merge(recs2, weights, all.x = TRUE)

avg_kwh_r = avg_kwh_r[, .(avg_kwh_r = sum(value*KWH) / sum(value)), by = .(division, weight)]

#standard error: -------------------------------------------
setkey(avg_kwh_r, division)
setkey(avg_kwh)
avg_kwh_t = merge(avg_kwh_r, avg_kwh, all.x = TRUE)

avg_kwh_tt = avg_kwh_t[order(division), .(avg_kwh = avg_kwh[1], se_kwh = 2 * sqrt(mean({avg_kwh_r - avg_kwh}^2))), by = division
                      ][, `:=`(lwr = pmax(avg_kwh - qnorm(.975)*se_kwh, 0), upr = avg_kwh + qnorm(.975)*se_kwh), by = division
                        ][order(-avg_kwh), .(division, avg_kwh = round(avg_kwh, digits = 0), lwr = round(lwr, digits = 0), upr = round(upr, digits = 0))]
#table2-1
table2_1 = avg_kwh_tt[, .(division, point_estimate = sprintf('%4.0f(%4.0f, %4.0f)', avg_kwh, lwr, upr))] 
#plot2-1
plot2_1 = ggplot(avg_kwh_tt, aes(x = division, y = avg_kwh)) +
  geom_bar(stat="identity") +
  geom_errorbar( aes(ymin=lwr, ymax=upr), col='navy' ) + 
  xlab('Division') + 
  ylab('average electricity usage') +
  coord_flip()
  

#average total kwh of electricity usage by division, stratify by urban
#point estimate
avg_kwh_ub = recs[, `:=`(urban = UATYP10 %in% c('U', 'C'), division = factor(DIVISION, 1:10, divisions))
                  ][, .(avg_kwh = sum(KWH*NWEIGHT)/sum(NWEIGHT)), by = .(division, urban)
                    ][order(division), ]
#replicate weight
recs3 = recs[, `:=`(urban = UATYP10 %in% c('U', 'C'), division = factor(DIVISION, 1:10, divisions))
             ][ ,.(DOEID, division, NWEIGHT, urban, KWH)]

setkey(recs3, DOEID)
setkey(weights, DOEID)
avg_kwh_r_ub = merge(recs3, weights, all.x = TRUE)
avg_kwh_r_ub = avg_kwh_r_ub[, .(avg_kwh_r_ub = sum(value*KWH) / sum(value)), by = .(division, weight, urban)]

#standard error
setkeyv(avg_kwh_r_ub, c("division", "urban"))
setkeyv(avg_kwh_ub, c("division", "urban"))
avg_kwh_r_ub_m = merge(avg_kwh_r_ub, avg_kwh_ub, all.x = TRUE)

avg_kwh_ub_t = avg_kwh_r_ub_m[order(division, urban), .(avg_kwh_ub = avg_kwh[1], se_kwh = 2 * sqrt(mean({avg_kwh_r_ub - avg_kwh}^2))), by = .(division, urban)
                    ][, `:=`(lwr = pmax(avg_kwh_ub - qnorm(.975)*se_kwh, 0), upr = avg_kwh_ub + qnorm(.975)*se_kwh), by = .(division, urban)
                      ][order(division), .(division, AREA = factor(urban, labels = c("RURAL", "URBAN")), avg_kwh_ub = round(avg_kwh_ub, digits = 0), lwr = round(lwr, digits = 0), upr = round(upr, digits = 0))]

avg_kwh_ub_tt = avg_kwh_ub_t[, .(division, AREA, point_estimate = sprintf('%4.0f(%4.0f, %4.0f)', avg_kwh_ub, lwr, upr))] 

table2_2 = dcast(avg_kwh_ub_tt, division~AREA)

plot2_2 = ggplot(avg_kwh_ub_t, aes(x = division, y = avg_kwh_ub)) +
  geom_bar(aes(fill=AREA), stat="identity", position = position_dodge()) +
  geom_errorbar( aes(ymin=lwr, ymax=upr), col='navy', position = position_dodge(0.9), width = 0.5 ) + 
  xlab('Division') + 
  ylab('average electricity usage') +
  coord_flip()


#### part 3  #####
recs4 = recs0[ , `:=`(urban = UATYP10 %in% c('U', 'C'))
                      ][ ,.(DOEID, NWEIGHT, INTERNET, division, urban)]

#point estimate: -----------------------------------------
p_internet = recs4[order(division, urban), .(p_internet = sum(NWEIGHT*{INTERNET == 1}) / sum(NWEIGHT)), by = .(division, urban)]

#replicate weigth: -------------------------------------------
setkey(recs4, DOEID)
setkey(weights, DOEID)
p_internet_m = merge(recs4, weights, all.x = TRUE)
p_internet_r = p_internet_m[ , .(r_internet = sum(value*{INTERNET ==1}) / sum(value)), by = .(division, urban, weight)]

#standard error: ---------------------------------------
setkeyv(p_internet_r, c('division', 'urban'))
setkeyv(p_internet, c('division', 'urban'))
p_internet_m2 = merge(p_internet_r, p_internet, all.x = TRUE)

p_internet_t = p_internet_m2[order(division), .(p_internet = p_internet[1], se_internet = 2 * sqrt(mean({r_internet - p_internet}^2))), by = .(division, urban)
                          ][, `:=`(lwr = pmax(p_internet - qnorm(.975)*se_internet, 0), upr = p_internet + qnorm(.975)*se_internet), by = .(division, urban)
                            ][ , .(division, AREA = factor(urban, labels = c("RURAL", "URBAN")), p_internet = round(p_internet*100, digits = 2), lwr = round(lwr*100, digits = 2), upr = round(upr*100, digits = 2))]

p_internet_tt = p_internet_t[ , .(division, AREA, p_internet, point_estimate = sprintf('%4.1f%%(%4.1f, %4.1f)', p_internet, lwr, upr))]

p_internet2 = dcast(p_internet_tt, division ~ AREA)
p_internet2 = data.table(p_internet2)

## calculate disparity ##
disparity = p_internet_t[, .(division, AREA, p_internet)]
disparity = dcast(disparity, division ~ AREA)
disparity = data.table(disparity)
disparity = disparity[, .(division, DISPARITY = (URBAN - RURAL))
                      ][order(DISPARITY), ]

setkey(p_internet2, 'division')
setkey(disparity, 'division')
table3 = merge(p_internet2, disparity, all.x = TRUE)
#greatest disparity = Mountain South (18.6)


plot3 = ggplot(p_internet_t, aes(x = division, y = p_internet, fill = AREA)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar( aes(ymin=lwr, ymax=upr), col='navy', position = position_dodge(0.9), width = 0.5) + 
  xlab('Division') + 
  ylab('% of homes with internet access') +
  coord_flip()

#### part 4 ####

# region: -------------------------------------------
region = c("Northeast",
           "Midwest",
           "South",
           "West")
recs4 = recs[, `:=`(region = factor(REGIONC, 1:4, region))]


#point estimate: -------------------------------------------
p_cera = recs0[, .(DOEID, region, NWEIGHT, ROOFTYPE), by = region
                 ][order(region), .(p_cera = sum(NWEIGHT*{ROOFTYPE == 1}) / sum(NWEIGHT)), by = region]

#replicate weigth: -------------------------------------------
recs4 = recs4[ ,.(DOEID, region, NWEIGHT, ROOFTYPE)]
setkey(recs4, DOEID)
setkey(weights, DOEID)
p_cera_r = merge(recs4, weights, all.x = TRUE)

p_cera_r = p_cera_r[, .(r_cera = sum(value*{ROOFTYPE == 1}) / sum(value)), by = .(region, weight)]

#standard error: -------------------------------------------
setkey(p_cera_r, region)
setkey(p_cera, region)
p_cera_r = merge(p_cera_r, p_cera, all.x = TRUE)

p_cera = p_cera_r[order(p_cera), .(p_cera = p_cera[1], se_cera = 2 * sqrt(mean({r_cera - p_cera}^2))), by = region
                      ][, `:=`(lwr = pmax(p_cera - qnorm(.975)*se_cera, 0), upr = p_cera + qnorm(.975)*se_cera), by = region
                        ][order(p_cera), .(region, p_cera = p_cera*100, lwr = lwr*100, upr = upr*100)]

table4 = p_cera[, .(region, point_estimate = sprintf('%4.1f%%(%4.1f, %4.1f)', p_cera, lwr, upr))]

#largest = West; lowest: Midwest

#graph
plot4 = ggplot(p_cera,  aes(x = region, y = p_cera)) +
  geom_bar(stat="identity") +
  geom_errorbar( aes(ymin=lwr, ymax=upr), col='navy' ) + 
  xlab('Region') + 
  ylab('% of homes having Ceramic or clay tiles as major rooftype') 

