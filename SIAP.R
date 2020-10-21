library(dplyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(tidyverse)

SIAP <- read.csv("/Users/erikaluna/Dropbox/UBC/Research/thesis/Data/Agt_cierre_80_16_equivalenciesFAO-SIAP.csv") 


SIAP %>% 
  #group_by() %>% 
  top_n(n = 10, wt = Valor) %>% 
  ggplot(aes(x = YearAgricola, y = Valor, colour = new.crop)) +
  geom_line() 
  #scale_x_discrete(name="Crops")# +
  #scale_y_continuous(labels = scales::comma)

#+
  facet_grid( ~ ., scales = "free_y", 
             labeller = label_value,
             switch = "x") +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        text = element_text(size=15),
        axis.title.y = element_blank(),
        strip.text = element_text(size = rel(1))) +
  guides(color = FALSE)

SIAP %>% 
  top_n(n=10, wt=Valor) %>% 
  ggplot(SIAP, aes(YearAgricola, Valor, colour=new.crop, group=new.crop)) +
  geom_line()
      
  pd=position_dodge(0.9)
tmp <- ggplot(SIAP, aes(YearAgricola, Valor, colour=new.crop, group=new.crop)) +
    stat_summary(fun.y=mean, geom="line", position=pd) +
    #stat_summary(fun.y=mean, geom="point", position=pd) +
    theme_bw()
tmp

# add variable "area"
SIAP <- SIAP %>% 
  mutate(area = round(production / yield, digits = 2))


write.csv(SIAP, file = "SIAP.csv")
