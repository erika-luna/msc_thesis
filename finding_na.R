# Missing data

library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(DT)

SIAP <- read.csv("/Users/erikaluna/R\ Studio/msc_thesis/SIAP.csv") 






#round(x, digits = 0)
#AGS <- SIAP %>% 
tmp <- SIAP %>% 
#BC <- SIAP %>% 
  filter(state == "campeche", crop == "mango") %>% 
  #filter(state == "aguascalientes", type == "food") %>% 
  #filter(state == "baja california", type == "food") %>% 
  #select(crop:year)# %>% 
  group_by(crop, year) %>% 
  summarise(ag_yield = sum(production)/sum(area),
            ag_prod = sum(production),
            ag_planted = sum(planted),
            ag_harv = sum(harvested), 
            ag_losses = sum(losses))
 #summarise(sum_prod = sum(production), .groups = 'drop')# %>% 
  #summarise(n_distinct(crop))
 # DT::datatable()

write.csv(tmp, file = "SIAP_mango.csv")


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
  #summarise(sum_prod = sum(production), .groups = 'drop')# %>% 
  summarise(ag_yield = sum(production)/sum(harvested),
            ag_prod = sum(production),
            ag_planted = sum(planted),
            ag_harv = sum(harvested), 
            ag_losses = sum(losses))


crop_full <- one_crop %>% 
  group_by(state) %>% 
  summarise(obs = sum(!is.na(ag_prod)))

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
  summarise(obs = sum(!is.na(ag_prod)))

number_obs  %>% 
  DT::datatable()


plot_mango <- number_obs %>% 
  ggplot(aes(state, obs)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(name = "number of observations") 
plot_mango

mango_complete <- number_obs %>% 
  filter(obs > 34)
mango_complete %>% 
  DT::datatable()
#mango_states_complete <- droplevels(mango$state == "baja california")

mango_ts <- mango %>% 
  #ggplot(aes(year, ag_prod)) + 
  #ggplot(aes(year, ag_yield)) +
  #ggplot(aes(year, ag_planted)) + 
  #ggplot(aes(year, ag_harv)) + 
  #ggplot(aes(year, ag_losses)) + 
  geom_line()+
  #ylab("Production (tonnes)") +
  ylab("Losses (ha)") +
  xlab("Years") +
  #ggtitle("Mango Production 1980 - 2016") +
  ggtitle("Mango - Losses (planted - harvested) 1980 - 2016") +
  geom_rect(data = subset(mango, state %in% c(mango_complete$state)), 
            fill = NA, colour = "red", xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +     
  facet_wrap(~state, scales="free_y", ncol=5) 
  #facet_wrap(~state, ncol=5)
mango_ts  
#ggsave("mango_prod.png")
ggsave("mango_losses.png")

summary(mango)

write.csv(mango, file = "mango.csv")

#### Descriptive Statistics ####

mango %>% 
  group_by(state) %>% 
  summarise(max_prod = max(ag_prod, na.rm=T),
            min_prod = min(ag_prod, na.rm=T),
            range_prod = max(ag_prod, na.rm=T) - min(ag_prod, na.rm=T),
            sd_prod = sd(ag_prod, na.rm=T),
            mean_prod = mean(ag_prod, na.rm=T),
            median_prod = median(ag_prod, na.rm=T))


mango %>% 
  ggplot(aes(state, ag_prod)) +
  geom_boxplot()


#tapply(mango$ag_prod, mango$state, summary)
