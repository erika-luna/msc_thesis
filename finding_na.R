# Missing data

library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)


AGS <- SIAP %>% 
  #filter(state == "aguascalientes", crop == "aceituna") %>% 
  filter(state == "aguascalientes") %>% 
  #select(crop:year)# %>% 
  group_by(crop, year) %>% 
 summarise(sum_prod = sum(production), .groups = 'drop')# %>% 
  #summarise(n_distinct(crop))
 # DT::datatable()

period <- tibble(c(1980:2016))
  
#period <- tibble(rep(c(1980:2016), times = 90))
colnames(period) <- c("year")


amigo <- left_join(period, AGS)
#tmp <- left_join(period, AGS)
#tmp <- union_all(period, AGS)
#tmp <- cbind(AGS, period)

amiga <- amigo %>% 
  group_by(crop) %>% 
  #summarise(mean_prod = mean(sum_prod)) %>% 
  summarise(obs = sum(!is.na(sum_prod)))  # of non-NA’s )
 # DT::datatable()
  
AGS_obs <- amiga %>% 
  ggplot(aes(crop, obs)) +
  geom_bar(stat = "identity") +
AGS_obs


tmp <- tmp %>% 
  group_by(crop, year)
  
str(AGS$crop)    


