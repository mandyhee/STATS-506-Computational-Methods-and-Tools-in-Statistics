q3 <- read.csv("recs2015_public_v3.csv", header = T)
weight <- q3 %>%
  dplyr::select(DOEID, BRRWT1:BRRWT96)

#3(a)
#wall type proportion among each division
wall_type_prop <- q3 %>% 
  dplyr::select(DIVISION, WALLTYPE, NWEIGHT) %>%
  complete(DIVISION, WALLTYPE) %>%
  replace_na( list(NWEIGHT = 0) ) %>%
  group_by(DIVISION, WALLTYPE) %>%
  summarize(wall=sum(NWEIGHT)) %>%
  group_by(DIVISION) %>%
  mutate( pct = 100*wall / sum(wall) ) 

#convert weights to long
weights_long = weight %>% 
  gather(key = 'repl', value = 'w', BRRWT1:BRRWT96)

#key value for each observation
wall_type = q3 %>% 
  select(DOEID, DIVISION, WALLTYPE, NWEIGHT) %>%
  replace_na( list(Weight=0) ) %>%
  group_by(DIVISION, WALLTYPE)

#join wall type & weights
wall_type_rep <- 
  weights_long %>% 
  left_join(wall_type %>% mutate( DOEID=as.integer(DOEID) ) , by='DOEID' )


# Replicate weighted proportions: --------------------------------------------
wall_type_prop_repl <- 
  wall_type_rep %>%
  group_by(DIVISION, WALLTYPE, repl) %>%
  summarize(Homes_r=sum(w)) %>%
  group_by(DIVISION, WALLTYPE) %>%
  mutate( pct_r = 100*Homes_r / sum(Homes_r) ) 

wall_type_prop_repl <-
  wall_type_prop_repl %>% 
  ungroup() %>%
  left_join(wall_type_prop, by = c('DIVISION', 'WALLTYPE') )

#calculate replicate standard error and CI
wall_type_prop <-
  wall_type_prop_repl %>%
  group_by(DIVISION, WALLTYPE) %>%
  summarize( pct = pct[1],
             std_err = 2 * sqrt( mean( {pct_r - pct}^2 ) )
  ) %>%
  mutate( lwr = pct - qnorm(.975)*std_err,
          upr = pct + qnorm(.975)*std_err
  )

#extract proportion of home having stucco construction as major outside wall material
stucco_wall_prop <- wall_type_prop %>%
  filter(WALLTYPE == 4) %>%
  arrange(pct) %>%
  mutate(pct = round(pct, digits = 2), std_err = round(std_err, digits = 2), 
         lwr = round(lwr, digits = 2), upr = round(std_err, digits =2))


#highest: division 9, lowest: division 6
sqldf("select DIVISION, max(pct) as max_percentage from stucco_wall_prop")
sqldf("select DIVISION, min(pct) as min_percentage from stucco_wall_prop")

#plot
library(ggplot2)
g_q3_1 <- ggplot(stucco_wall_prop, aes(x = DIVISION, y = pct)) + 
  geom_point( col='red', pch = 15, cex=2) + 
  geom_errorbar( aes(ymin=lwr, ymax=upr), col='navy' ) + 
  xlab('DIVISION') + 
  ylab('% having STUCCO as major walltype') +
  scale_x_continuous(breaks=seq(0,10,1))



#3(b)
#average total electricity usage in kilowatt hours in each division
q3_2_1 <- q3 %>%
  dplyr::select(DIVISION, KWH) %>%
  group_by(DIVISION) %>%
  summarise(mean_elec = mean(KWH, na.rm=T), sd_elec = sd(KWH, na.rm = T), n_elec = n()) %>%
  mutate(se_elec = sd_elec / sqrt(n_elec),
       lwr = mean_elec - dnorm(0.975) * se_elec,
       upr = mean_elec + dnorm(0.975) * se_elec,
       mean_elec = round(mean_elec, digits = 2),
       sd_elec = round(sd_elec, digits = 2),
       lwr = round(lwr, digits = 2),
       upr = round(upr, digits = 2))
#plot
g_q3_2 <- ggplot(q3_2_1, aes(x = DIVISION, y = mean_elec)) + 
  geom_point( col='red', pch = 15, cex=2) + 
  geom_errorbar( aes(ymin=lwr, ymax=upr), col='navy' ) + 
  xlab('DIVISION') + 
  ylab('average electricity usage') +
  scale_x_continuous(breaks=seq(0,10,1))

#average total electricity usage in kilowatt hours in each division, stratify by urban or rural
q3_2_2 <- q3 %>%
  dplyr::select(DIVISION, UATYP10, KWH) %>%
  arrange(DIVISION, UATYP10) %>%
  mutate(type = factor(UATYP10, levels = c("U", "C", "R"), labels = c("Urban", "Urban", "Rural"))) %>%
  group_by(DIVISION, type) %>%
  summarise(mean_elec = mean(KWH, na.rm=T), sd_elec = sd(KWH, na.rm = T), n_elec = n()) %>%
  mutate(se_elec = sd_elec / sqrt(n_elec),
         lwr = mean_elec - dnorm(0.975) * se_elec,
         upr = mean_elec + dnorm(0.975) * se_elec,
         mean_elec = round(mean_elec, digits = 2),
         sd_elec = round(sd_elec, digits = 2),
         lwr = round(lwr, digits = 2),
         upr = round(upr, digits = 2)) 

#plot: urban

g_q3_2_2 <- ggplot(q3_2_2, aes(x = DIVISION, y = mean_elec, fill = type)) + 
  geom_point( aes(col= type), pch = 15, cex=2) + 
  geom_errorbar( aes(ymin=lwr, ymax=upr), col='navy' ) + 
  xlab('DIVISION') + 
  ylab('average electricity usage') +
  scale_x_continuous(breaks=seq(0,10,1))



#3(c)

#calculate proportion 
ua_type_prop <- q3 %>%
  dplyr::select(DIVISION, UATYP10, INTERNET, NWEIGHT) %>%
  arrange(DIVISION, UATYP10) %>%
  mutate(type = factor(UATYP10, levels = c("U", "C", "R"), labels = c("Urban", "Urban", "Rural"))) %>%
  mutate(INTERNET = factor(INTERNET, levels = c(1,0), labels = c("internet_1", "internet_0"))) %>%
  group_by(DIVISION, type, INTERNET) %>%
  #count INTERNET in each type by division
  summarise(total=sum(NWEIGHT)) %>%
  #long to wide format, by INTERNET
  tidyr::spread(key = INTERNET, value = total) %>%
  mutate(pct = internet_1 / (internet_1 + internet_0), proportion = round(pct, digits = 2)) %>%
  select(DIVISION, type, pct) 

  
#convert weights to long
weights_long = weight %>% 
  gather(key = 'repl', value = 'w', BRRWT1:BRRWT96)

#key value for each observation
ua_type = q3 %>% 
  select(DOEID, DIVISION, UATYP10, INTERNET, NWEIGHT) %>%
  replace_na( list(Weight=0) ) %>%
  group_by(DIVISION,UATYP10)


#join wall type & weights
ua_type_rep <- 
  weights_long %>% 
  left_join(ua_type %>% mutate( DOEID=as.integer(DOEID) ) , by='DOEID' ) %>%
  mutate(type = factor(UATYP10, levels = c("U", "C", "R"), labels = c("Urban", "Urban", "Rural"))) %>%
  mutate(INTERNET = factor(INTERNET, levels = c(1,0), labels = c("internet_1", "internet_0"))) %>%
  select(-UATYP10)
  
    

# Replicate weighted proportions: 
ua_type_prop_repl <- 
  ua_type_rep %>%
  group_by(DIVISION, type, repl) %>%
  summarize(total_r=sum(w)) %>%
  group_by(DIVISION, type) %>%
  mutate( pct_r = 100*total_r / sum(total_r)) 

ua_type_prop_repl <-
  ua_type_prop_repl %>% 
  ungroup() %>%
  left_join(ua_type_prop, by = c('DIVISION', 'type'))

#calculate replicate standard error and CI
ua_type_prop <-
  ua_type_prop_repl %>%
  group_by(DIVISION, type) %>%
  summarize(pct = pct[1],
             std_err = 2 * sqrt(mean({pct_r - pct}^2 ))) %>%
  mutate(lwr = pct - qnorm(.975)*std_err,
        upr = pct + qnorm(.975)*std_err,
         pct = round(pct, digits = 2),
         std_err = round(std_err, digits =2),
         lwr = round(lwr, digits = 2),
        upr = round(upr, digits = 2))

#plot: proportion of homes with internet access
g_q3_3 <- ggplot(ua_type_prop, aes(x = DIVISION, y = pct, fill = type)) + 
  geom_point(aes(col= type), pch = 15, cex=2) + 
  geom_errorbar(aes(ymin=lwr, ymax=upr), col='navy' ) + 
  xlab('DIVISION') + 
  ylab('proportion of homes with internet access') +
  scale_x_continuous(breaks=seq(0,10,1))

#calculate disparity
disparity <- ua_type_prop %>%
  dplyr::select(DIVISION, type, pct) %>%
  tidyr::spread(key = type, value = pct) %>%
  mutate(disparity = Urban - Rural) %>%
  arrange(disparity)

#select the largest disparity
sqldf("select DIVISION, max(disparity) from disparity")


