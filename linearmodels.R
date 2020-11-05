#### LINEAR MODEL ####
install.packages("robustbase")
install.packages("plotrix")
library(robustbase)
library(plotrix)
lmrob(ag_yield ~ year, lm, maize, ag_yield, state)

std.error(ags$ag_yield)



ags <- maize %>% 
  filter(state == "aguascalientes")

(my_lm <- lm(ag_yield~ ., data = ags))

(my_lm_otro <- lmrob(ag_yield ~ year, data = ags))
plot(my_lm_otro)

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





ggplot(aes(state, obs)) +
  geom_bar(stat = "identity")

#%>% 
  filter(complete.cases(.))
