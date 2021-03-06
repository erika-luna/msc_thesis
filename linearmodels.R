#### LINEAR MODEL ####
install.packages("robustbase")
install.packages("plotrix")
library(robustbase)
library(plotrix)
library(tidyverse)
lmrob(ag_yield ~ year, lm, maize, ag_yield, state)

std.error(ags$ag_yield)



ags <- mango %>% 
  filter(state == "chiapas")

write.csv(ags, "chiapas.csv")

(my_lm <- lm(ag_yield~ ., data = ags))

(my_lm_otro <- lmrob(ag_yield ~ year, data = ags))
plot(my_lm_otro)

my_lm_otro <- as.data.frame(my_l)

summary(my_lm_otro)
plot(my_lm)


fit = lm(ag_yield ~ .,data = ags)
se <- sqrt(diag(vcov(fit)))
plot(fit)



plot(ags)
lmST <-    lm(ag_yield ~ year, data = ags)
(RlmST <- lmrob(ag_yield ~ year, data = ags))
abline(lmST, col = "red")
abline(RlmST, col = "blue")

mpvals <-
  sapply(
    list(lm(ag_yield ~ year,  data = maize),
         lm(ag_prod ~ year, data = maize),
         lm(ag_losses ~ year,   data = maize)),
    extract_fpvalue)

mpvals <-
  sapply(
    list(lm(mpg ~ cyl_factor,  data = mtcars2),
         lm(disp ~ cyl_factor, data = mtcars2),
         lm(wt ~ cyl_factor,   data = mtcars2)),
    extract_fpvalue)

library(MASS)
library(sfsmisc)
summary(rsl <- rlm(ag_yield ~ ., maize))


### Simple linear regression
names(ags)
plot(ag_yield~ag_harv,ags)
fit1=lm(ag_yield~year,data=ags)
fit1
summary(fit1)
abline(fit1,col="red")
names(fit1)
confint(fit1)
predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")


library(broom)
tidy(my_lm)
coef(summary(my_lm))
coef(summary(my_lm_otro))
glance(my_lm_otro)
glance(my_lm)

ggplot(ags, aes(x = year, y = ag_yield)) +
  #geom_point() +
  geom_smooth(method="lm") 

maize_ags <- maize %>% 
  filter(state == "aguascalientes")

maize_ags %>% 
  ggplot(aes(year, ag_yield)) + 
  geom_point()

ggplot(maize_ags, aes(year,ag_yield)) +
  geom_point() +
  geom_smooth(method = "lm", se = T)

augment(my_lm_otro)

lm1 <- maize %>% 
  ggplot(aes(year, ag_yield)) + 
  geom_point() +
  geom_smooth() +
  ylab("Yield (tonnes/ha)") +
  xlab("Years") +
  ggtitle("Maize Yields") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  geom_rect(data = subset(maize, state %in% c(maize_complete$state)), 
            fill = NA, colour = "red", xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +     
  facet_wrap(~state, scales="free_y", ncol=5) 
lm1

maize_lm <- maize %>% 
  ggplot(aes(year, ag_yield)) + 
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  ylab("Yield (tonnes/ha)") +
  xlab("Years") +
  ggtitle("Maize Yields") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  geom_rect(data = subset(maize, state %in% c(maize_complete$state)), 
            fill = NA, colour = "red", xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +     
  facet_wrap(~state, scales="free_y", ncol=5) 
maize_lm

mango_lm <- mango %>% 
  ggplot(aes(year, ag_yield)) + 
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  ylab("Yield (tonnes/ha)") +
  xlab("Years") +
  ggtitle("mango Yields") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  geom_rect(data = subset(mango, state %in% c(mango_complete$state)), 
            fill = NA, colour = "red", xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf) +     
  facet_wrap(~state, scales="free_y", ncol=5) 
mango_lm

# Scatter plot with correlation coefficient
#:::::::::::::::::::::::::::::::::::::::::::::::::
sp <- ggscatter(ags, x = "year", y = "ag_yield",
                add = "reg.line",  # Add regressin line
                add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                conf.int = TRUE # Add confidence interval
)


ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    geom_smooth(method = "lm", se = T) +
    #stat_smooth(method = "lm", col = "red") +
    facet_wrap(~state, scales="free_y", ncol=5) +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
    
}
fit1 <- lm(ag_yield ~ state, data = maize)
ggplotRegression(fit1)


library("ggplot2")
library("datasets")
library("plyr")

#2.2. Create function.
regression=function(df){
  #setting the regression function. 
  reg_fun<-lm(formula=maize$ag_yield~maize$year) #regression function
  #getting the slope, intercept, R square and adjusted R squared of 
  #the regression function (with 3 decimals).
  slope<-round(coef(reg_fun)[2],3)  
  intercept<-round(coef(reg_fun)[1],3) 
  R2<-round(as.numeric(summary(reg_fun)[8]),3)
  R2.Adj<-round(as.numeric(summary(reg_fun)[9]),3)
  c(slope,intercept,R2,R2.Adj)
}
#3. Now let's subset the data frame base on the values of the supplement type (supp) and
#let's apply the function "regression" to every data frame. To achieve this
#let's use the "ddply" function from the "plyr". The results in this case is another
#data frame (regressions_data) whose rows are the valeus of the slope, intercept, 
#R_squared and R_squared_adj for every data frame.
regressions_data<-ddply(maize,"state",regression)
colnames(regressions_data)<-c ("supp","slope","intercept","R2","R2.Adj")

#4. Let's plot all the plots and the values of the regression models. 
qplot(year, ag_yield, data = maize, size=I(2))+geom_smooth(method="lm")+
  facet_wrap(~state, scales="free_y", ncol=5)+
  ggtitle("Regressions")+
  geom_label(data=regressions_data, inherit.aes=FALSE, aes(x = 1.2, y = 32,
                                                           label=paste("slope=",slope,","," ","intercept=",intercept,","," ","R^2=",R2,","," ","R^2.Adj=",R2.Adj)))



facet_wrap(~state, scales="free_y", ncol=5, labeller = r2_labeller) 

library(dplyr)


fitted_models = maize %>% group_by(state) %>% do(model = lm(ag_yield ~ year, data = .))
fitted_models
library(broom)
v <- fitted_models %>% tidy(model) %>% as_tibble()
v %>% 
  DT::datatable()

v %>% 
  summarise(zia = 2*std.error)

#### Complete cases ####
library(dplyr)


state_level <- SIAP %>% 
  select(crop, year, state, cycle, water, harvested, losses, production, yield) %>% 
  filter(year < 2002) 
mun_level <- SIAP %>% 
  select(crop, year, state, cycle, water, harvested, losses, production, yield) %>% 
  filter(year > 2001) 


missing_data <- 
  #state_level %>% 
  mun_level %>% 
  group_by(crop) %>%       
  summarise(count = n()) %>%
  arrange(desc(count))

tmp <- missing_data %>% 
  #filter(count > 600) #state
  filter(count > 2000) #mun

h <- tmp %>% 
  ggplot(aes(reorder(crop, -count), count, fill = crop)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("count") +
  xlab("crops") +
  #ggtitle("Data at the state level") +
  ggtitle("Data at the municipal level") +
  geom_col()
h


#### Production and Harvested Area ####
 
ylim.prim <- c(0, 100000000)   # in this example, precipitation
ylim.sec <- c(0, 150000)    # in this example, temperature

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])


ggplot(maize_nat, aes(year, ag_prod)) +
  geom_line() +
  geom_line(aes(y = ag_losses), color = "red") +
  scale_y_continuous("Production (tonnes)", trans = 'log10', labels = scales::comma, 
                     sec.axis = sec_axis(~ (. - a)/b, name = "Lost Area (ha) ", labels = scales::comma)) +
  scale_x_continuous("Year") +
  ggtitle("Maize Production and Lost Area")

ggplot(maize_nat, aes(year, ag_prod)) +
  geom_line() +
  geom_line(aes(y = ag_harv), color = "red") +
  scale_y_continuous("Production (tonnes)", trans = 'log10', labels = scales::comma, 
                     sec.axis = sec_axis(~ (. - a)/b, name = "Harvested (ha) ", labels = scales::comma)) +
  scale_x_continuous("Year") +
  ggtitle("Maize Production and Harvested Area")

ggplot(maize, aes(year, ag_prod)) +
  geom_line() +
  geom_line(aes(y = ag_harv), color = "red") +
  scale_y_continuous("Production (tonnes)", labels = scales::comma,
                     sec.axis = sec_axis(~ (. - a)/b, name = "Harvested (ha) ", labels = scales::comma)) +
  scale_x_continuous("Year") +
  ggtitle("Maize Production and Harvested Area") +
  facet_wrap(~state, scales="free_y") 

#### Maize Production Harvested and Lost Area ####
maize %>% 
  #filter(state %in% c("baja california","baja california sur", "chihuahua", "durango", "nayarit", "sinaloa", "sonora")) %>% 
  ggplot(aes(year, ag_prod)) +
  geom_line() +
  geom_line(aes(y = ag_losses), color = "red") +
  #geom_line(aes(y = ag_harv), color = "red") +
  scale_y_continuous("Production (tonnes)", trans = "log10", labels = scales::comma,
                     sec.axis = sec_axis(~ (. - a)/b, name = "Losses (ha) ", labels = scales::comma)) +
                     #sec.axis = sec_axis(~ (. - a)/b, name = "Harvested Area (ha) ", labels = scales::comma)) +
  scale_x_continuous("Year") +
  ggtitle("Maize Production and Lost Area") +
  #ggtitle("Maize Production and Harvested Area") +
  theme(legend.title = element_text(colour="blue", size=10, 
                                   face="bold")) +
  facet_wrap(~state, scales="free_y") 


ylim.prim <- c(0, 400000)   # in this example, precipitation
ylim.sec <- c(0, 40000)    # in this example, temperature

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])
#### Mango Production Harvested and Lost Area ####
mango %>% 
  filter(state %in% c("baja california sur", "durango", "nayarit", "sinaloa", "sonora")) %>% 
  ggplot(aes(year, ag_prod)) +
  geom_line() +
  geom_line(aes(y = ag_harv), color = "red") +
  scale_y_continuous("Production (tonnes)", trans = "log10", labels = scales::comma,
                     sec.axis = sec_axis(~ (. - a)/b, name = "Harvested Area (ha) ", labels = scales::comma)) +
  scale_x_continuous("Year") +
  ggtitle("Mango Production and Harvested Area") +
  facet_wrap(~state, scales="free_y") 


##### Histograms ####
qplot(maize$ag_losses,
      geom="histogram", 
      binwidth=10000)
ggplot(data=maize, aes(maize$ag_yield)) + 
  geom_histogram(aes(y =..density..), 
                 breaks=seq(1, 10, by = 1), 
                 #col="red", 
                 #fill="green", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title="Histogram for Age", x="Yields", y="Count")





#### Explore maize ####

maize %>% 
  group_by(state) %>% 
  summarise(max_yield = max(ag_yield, na.rm=T),
            min_yield = min(ag_yield, na.rm=T),
            range_yield = max(ag_yield, na.rm=T) - min(ag_yield, na.rm=T),
            sd_yield = sd(ag_yield, na.rm=T),
            mean_yield = mean(ag_yield, na.rm=T),
            median_yield = median(ag_yield, na.rm=T))
maiz <- SIAP %>% 
  filter(crop == "maiz") %>% 
  group_by(year, water) %>% 
  summarise(max_yield = max(yield, na.rm=T),
          min_yield = min(yield, na.rm=T),
          range_yield = max(yield, na.rm=T) - min(yield, na.rm=T),
          sd_yield = sd(yield, na.rm=T),
          mean_yield = mean(yield, na.rm=T),
          median_yield = median(yield, na.rm=T))

# Plot
maiz %>%
  ggplot( aes(x=year, y=mean_yield, group=water, color=water)) +
  geom_line()


elote <- SIAP %>% 
  filter(crop == "maiz") %>% 
  group_by(cycle, water, year) %>% 
  summarise(ag_yield = round(sum(production)/sum(harvested), digits = 2),
            ag_prod = sum(production),
            ag_planted = sum(planted),
            ag_harv = sum(harvested), 
            ag_losses = sum(losses),
            ag_har = round(sum(harvested)/sum(planted), digits = 2))

elote %>% 
  ggplot(aes(x=year, y=ag_har, color=water)) +
  geom_line()+
  geom_smooth(method = "lm", se = F) +
  #stat_cor(label.y = 4)+
  ylab("HAR") +
  xlab("Year") +
  facet_wrap(~cycle)

library(dplyr)
elotito <- elote  %>% 
  #gather("area", "ha", 4:5)
  gather("area", "ha", 5:6)

elotito %>% 
  ggplot(aes(x=year, y=ha, color=cycle)) +
  scale_y_continuous("Area (ha)", trans = 'log', labels = scales::comma) +
  geom_line()

trigo <- SIAP %>% 
  filter(crop == "trigo") %>% 
  group_by(year) %>% 
  summarise(ag_yield = round(sum(production)/sum(harvested), digits = 2),
            ag_prod = sum(production),
            ag_planted = sum(planted),
            ag_harv = sum(harvested), 
            ag_losses = sum(losses),
            ag_har = round(sum(harvested)/sum(planted), digits = 2))

trigo %>% 
  ggplot(aes(x=year, y=ag_har)) +
  geom_line()+
  geom_smooth(method = "lm", se = F) +
  #stat_cor(label.y = 4)+
  ylab("HAR") +
  xlab("Year") 

trigo %>% 
  ggplot(aes(x=year, y=ag_har, color=cycle)) +
  geom_line()+
  geom_smooth(method = "lm", se = F) +
  #stat_cor(label.y = 4)+
  ylab("Yield (t/ha)") +
  xlab("Year") 

mipan <- trigo  %>% 
  #gather("area", "ha", 4:5)
  gather("area", "ha", 5:6)

mipan %>% 
  ggplot(aes(x=year, y=ha, color=area)) +
  scale_y_continuous("Area (ha)", labels = scales::comma) +
  geom_line()

ylim.prim <- c(0, 1)   # in this example, precipitation
ylim.sec <- c(0, 200)    # in this example, temperature

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])
#### Mango Production Harvested and Lost Area ####
trigo %>% 
  #filter(state %in% c("baja california sur", "durango", "nayarit", "sinaloa", "sonora")) %>% 
  ggplot(aes(year, ag_har)) +
  geom_line() +
  geom_line(aes(y = ag_yield), color = "blue") +
  scale_y_continuous("HAR", labels = scales::comma,
                     sec.axis = sec_axis(~ (. - a)/b, name = "Yeld (t/ha) ", labels = scales::comma)) +
  scale_x_continuous("Year") #+
  #ggtitle("Mango Production and Harvested Area") +
  #facet_wrap(~state, scales="free_y") 


# Value used to transform the data
coeff <- 6

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(esquite, aes(x=year)) +
  
  geom_line( aes(y=ag_har), size=0.5, color=temperatureColor) + 
  geom_line( aes(y=ag_yield / coeff), size=0.5, color=priceColor) +
  
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "HAR",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Yield (t/ha)")
  ) + 
  
  #theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) #+
  
  ggtitle("Temperature down, price up")
  
  esquite <- SIAP %>% 
    filter(crop_var == "maiz palomero") %>%
    filter(cycle == "fall-winter")
  
  trigo <- SIAP %>% 
    filter(crop == "trigo") 
  
  trigo$crop_var <- factor(trigo$crop_var)
  trigo <- trigo %>% 
    #filter(crop_var = "trigo ornamental (manojo)") %>% 
    group_by(year, cycle) %>% 
    summarise(ag_yield = round(sum(production)/sum(harvested), digits = 2),
              ag_prod = sum(production),
              ag_planted = sum(planted),
              ag_harv = sum(harvested), 
              ag_losses = sum(losses),
              ag_har = round(sum(harvested)/sum(planted), digits = 2))
  
  
  
   # filter(crop_var == "maiz palomero") %>%
   # filter(cycle == "fall-winter")

  
  esquite$crop_var <- factor(esquite$crop_var)
  esquite <- esquite %>% 
    group_by(year) %>% 
    summarise(ag_yield = round(sum(production)/sum(harvested), digits = 2),
              ag_prod = sum(production),
              ag_planted = sum(planted),
              ag_harv = sum(harvested), 
              ag_losses = sum(losses),
              ag_har = round(sum(harvested)/sum(planted), digits = 2))
  
  esquite %>% 
    ggplot(aes(x=year, y=ag_yield)) +
    #scale_y_continuous("Area (ha)", trans = 'log', labels = scales::comma) +
    geom_line() +
    geom_smooth(method = "lm", se = F) 
  
  
  #### Mango ####
  
  manguito <- SIAP %>% 
    filter(crop == "mango") 
  
  manguito$crop_var <- factor(manguito$crop_var)
  
  manguito <- manguito %>% 
    group_by(state, year) %>% 
    summarise(ag_yield = round(sum(production)/sum(harvested), digits = 2),
              ag_prod = sum(production),
              ag_harv = sum(harvested))
    
  library(ggpubr)
  manguito %>% 
    ggplot(aes(x=year, y=ag_yield)) +
    #scale_y_continuous("Area (ha)", trans = 'log', labels = scales::comma) +
    geom_line() +
    
    geom_smooth(method = "lm", se = F) +
    stat_cor()+
    facet_wrap(~state, scales="free_y")
  
  #### Chile #####
  chile <- SIAP %>% 
    filter(crop == "chile") 
  
  chile$crop_var <- factor(chile$crop_var)
  
  chile <- chile %>% 
    group_by(state, year) %>% 
    summarise(ag_yield = round(sum(production)/sum(harvested), digits = 2),
              ag_prod = sum(production),
              ag_harv = sum(harvested))
  
  library(ggpubr)
  chile %>% 
    ggplot(aes(x=year, y=ag_yield)) +
    #scale_y_continuous("Area (ha)", trans = 'log', labels = scales::comma) +
    geom_line() +
    
    geom_smooth(method = "lm", se = F) +
    stat_cor()+
    facet_wrap(~state, scales="free_y")
  

  