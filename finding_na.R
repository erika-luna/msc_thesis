# Missing data

library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)


#AGS <- SIAP %>% 
BC <- SIAP %>% 
  #filter(state == "aguascalientes", crop == "aceituna") %>% 
  #filter(state == "aguascalientes", type == "food") %>% 
  filter(state == "baja california", type == "food") %>% 
  #select(crop:year)# %>% 
  group_by(crop, year) %>% 
 summarise(sum_prod = sum(production), .groups = 'drop')# %>% 
  #summarise(n_distinct(crop))
 # DT::datatable()

period <- tibble(c(1980:2016))
  
#period <- tibble(rep(c(1980:2016), times = 90))
colnames(period) <- c("year")


#amigo <- left_join(period, AGS)
amigo <- left_join(period, BC)
#tmp <- left_join(period, AGS)
#tmp <- union_all(period, AGS)
#tmp <- cbind(AGS, period)

amiga <- amigo %>% 
  group_by(crop) %>% 
  #summarise(mean_prod = mean(sum_prod)) %>% 
  summarise(obs = sum(!is.na(sum_prod)))  # of non-NA’s )
 # DT::datatable()
  
#AGS_obs <- amiga %>% 
BC_obs <- amiga %>% 
  ggplot(aes(crop, obs)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(name = "number of observations") 
#AGS_obs
BC_obs

#### state vs. number of obs for one crop ####

aceituna <- SIAP %>% 
  #filter(state == "aguascalientes", crop == "aceituna") %>% 
  #filter(state == "aguascalientes", type == "food") %>% 
  filter(crop == "aceituna") %>% 
  #select(crop:year)# %>% 
  group_by(year, state) %>% 
  summarise(sum_prod = sum(production), .groups = 'drop')# %>% 

aceituna_full <- na.omit(aceituna)
tmp <- aceituna_full %>% 
  group_by(state) %>% 
  summarise(obs = sum(!is.na(sum_prod)))


aceituna_obs <- aceituna %>% 
  group_by(state) %>% 
  summarise(obs = sum(!is.na(sum_prod)))

aceituna_state_obs <- aceituna_obs %>% 
  ggplot(aes(state, obs)) +
  geom_bar(stat = "identity")
aceituna_state_obs

#### 

one_crop <- SIAP %>% 
  #filter(state == "aguascalientes", crop == "aceituna") %>% 
  #filter(state == "aguascalientes", type == "food") %>% 
  filter(crop == "jitomate") %>% 
  #select(crop:year)# %>% 
  group_by(year, state) %>% 
  summarise(sum_prod = sum(production), .groups = 'drop')# %>% 

crop_full <- one_crop %>% 
  group_by(state) %>% 
  summarise(obs = sum(!is.na(sum_prod)))

plot_obs <- crop_full %>% 
  ggplot(aes(state, obs)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(name = "number of observations") 
plot_obs
####






