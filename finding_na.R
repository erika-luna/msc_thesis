# Missing data

library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)

SIAP <- read.csv("/Users/erikaluna/R\ Studio/msc_thesis/SIAP.csv") 
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
amigo <- left_join(period, BC$year)
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
  filter(crop == "mango") %>% 
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

##### A data frama with all years and all states for states that report mango production #####
period <- tibble(rep(c(1980:2016), times = 26)) #26 states report mango production
colnames(period) <- c("year") 
states <- tibble(rep(c("baja california", "baja california sur", "campeche", 
                       "chiapas", "colima", "durango",
                        "guanajuato", "guerrero", "hidalgo", "jalisco", 
                       "mexico", "michoacan", "morelos", "nayarit", "oaxaca", 
                       "puebla", "queretaro", "quintana roo",
                        "san luis potosi", "sinaloa", "sonora", "tabasco", 
                       "tamaulipas", "veracruz", "yucatan", "zacatecas"), times = 37))
colnames(states) <- c("state") 
states <- states %>% 
  arrange(state)
states_period <- cbind(states, period)

##### Mangoes data frame #####

mango <- left_join(states_period, one_crop, by=c("state", "year"))
mango <- mango %>%  
  transform(i=as.numeric(factor(state))) %>% 
  transform(t=as.numeric(factor(year))) %>% 
  group_by(year) %>% 
  arrange(state)

number_obs <- mango %>% 
  group_by(state) %>% 
  summarise(obs = sum(!is.na(sum_prod)))

plot_mango <- number_obs %>% 
  ggplot(aes(state, obs)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(name = "number of observations") 
plot_mango

mango_complete <- number_obs %>% 
  filter(obs > 34)

#mango_states_complete <- droplevels(mango$state == "baja california")

mango_ts <- mango %>% 
   ggplot(aes(year, sum_prod, color=state)) + 
  geom_line()
mango_ts  

summary(mango)

write.csv(mango, file = "mango.csv")

