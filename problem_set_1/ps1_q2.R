library(dplyr)
library(tidyr)
library(nycflights13)
setwd("~/Desktop/STAT506/ps1")

#(a)
#count total flights
q2_1_1 <- flights %>%  
  filter(month <= 10)  %>%
  summarise(total = n())

#filter airlines that is responsible for at least 1% of flights
q2_1_2 <- flights %>%
  filter(month <= 10)  %>%
  select(carrier, flight) %>%
  group_by(carrier) %>% 
  summarise(n = n()) %>%
  filter(n >= 2813) %>%
  rename(n_2013=n) %>%
  mutate(p_2013 = n_2013 / 281373, p_2013 = round(p_2013, digits = 3),
         se_2013 = sqrt(p_2013*(1-p_2013)/n_2013), se_2013 = round(se_2013, digits = 3), 
         CI_lwr_2013 = p_2013 - qnorm(.975)*se_2013, CI_lwr_2013 = round(CI_lwr_2013, digits = 3),
         CI_upr_2013 = p_2013 + qnorm(.975)*se_2013, CI_upr_2013 = round(CI_upr_2013, digits = 3))



#(b)
q2_2_2 <- read.csv("flights14.csv", header=T)

#calculate percentage for 2014
q2_2_3 <- q2_2_2 %>%
  filter(month <= 10)  %>%
  filter(!is.na(flight)) %>%
  dplyr::select(carrier, flight) %>%
  group_by(carrier) %>% 
  summarise(n = n()) %>%
  rename(n_2014=n) %>%
  mutate(p_2014 = n_2014 / sum(n_2014), p_2014 = round(p_2014, digits = 3),
         se_2014 = sqrt(p_2014*(1-p_2014)/n_2014), se_2014 = round(se_2014, digits = 3), 
         CI_lwr_2014 = p_2014 - qnorm(.975)*se_2014, CI_lwr_2014 = round(CI_lwr_2014, digits = 3),
         CI_upr_2014 = p_2014 + qnorm(.975)*se_2014, CI_upr_2014 = round(CI_upr_2014, digits = 3))

# {merge 2013, 2014}: add airline full name, create percent_change

merge1 <- merge(q2_1_2, q2_2_3, by = "carrier")

merge1 <- merge(merge1, airlines, by = "carrier")  #get full airline names

merge1$p_change = merge1$p_2014 - merge1$p_2013   #create percent_change (2014-2013)
merge1$n_change = merge1$n_2014 - merge1$n_2013   #create number_change (2014-2013)

#airline with largest increase
sqldf("select carrier, name, max(p_change) from merge1")
#airline with smallest increase
sqldf("select carrier, name, min(p_change) from merge1")


#(c)
#count flights within each airline by origin --> numerator
q2_3_1 <- flights %>%
  filter(carrier %in% c("9E", "AA", "B6", "DL", "EV", "FL", "MQ", "UA", "US", "VX", "WN")) %>%
  select(origin, carrier, flight) %>%
  group_by(origin, carrier) %>%
  summarise(n_flights = n()) 

#count total flights by origin (for calculate proportion) --> denominator
q2_3_2 <- flights %>%
  filter(carrier %in% c("9E", "AA", "B6", "DL", "EV", "FL", "MQ", "UA", "US", "VX", "WN")) %>%
  select(origin, flight) %>%
  group_by(origin) %>%
  summarise(total = n())

#merge and calculate proportion, SE & 95%CI
merge2 <- merge(q2_3_1, q2_3_2, by = "origin", all = T)
merge2 <- merge2 %>%
  mutate (proportion = n_flights / total, proportion = round(proportion, digits = 3),
          se = sqrt(proportion*(1-proportion)/n_flights), se = round(se, digits = 3), 
          CI_lwr = proportion - qnorm(.975)*se, CI_lwr = round(CI_lwr, digits = 3),
          CI_upr = proportion + qnorm(.975)*se, CI_upr = round(CI_upr, digits = 3))

#Which airline is the largest carrier at each airport?
sqldf("select origin, carrier, max(proportion) as max_ from merge2 group by origin")




