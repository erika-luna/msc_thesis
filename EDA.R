# Exploratory Data Analysis

# What type of variation occurs within my variables?
# What type of covariation occurs between my variables?

library(tidyverse)
library(ggplot2)

#### Preprocessesing ####

# Data: 
# From 1980 to 2002 the data is at State level
# From 2003 to 2016 the data is at Municipal level.

SIAP <- read.csv("/Users/erikaluna/Dropbox/UBC/Research/thesis/Data/Agt_cierre_80_16_equivalenciesFAO-SIAP.csv") 

vars_english <- c("crop_var", "crop", "year", "state_code", "state", "mun_code", "mun", "unit",
                  "cycle", "water", "planted", "harvested", "losses", "production", "yield", "pmr", "value", "type")

colnames(SIAP) <- vars_english

levels(SIAP$cycle)[levels(SIAP$cycle)=="otono - invierno"] <- "fall-winter"
levels(SIAP$cycle)[levels(SIAP$cycle)=="otono-invierno"] <- "fall-winter"
levels(SIAP$cycle)[levels(SIAP$cycle)=="primavera - verano"] <- "spring-summer"
levels(SIAP$cycle)[levels(SIAP$cycle)=="primavera-verano"] <- "spring-summer"
levels(SIAP$cycle)[levels(SIAP$cycle)=="perennes"] <- "perennial"

levels(SIAP$water)[levels(SIAP$water)=="riego"] <- "irrigated"
levels(SIAP$water)[levels(SIAP$water)=="temporal"] <- "rainfed"

# Translate crop names
# Cereals
levels(SIAP$crop)[levels(SIAP$crop)==""]

# Convert quant variables to numeric (R initially read them as factors)
SIAP$planted <- as.numeric(levels(SIAP$planted))[SIAP$planted]
SIAP$harvested <- as.numeric(levels(SIAP$harvested))[SIAP$harvested]
SIAP$losses <- as.numeric(levels(SIAP$losses))[SIAP$losses]
SIAP$production <- as.numeric(levels(SIAP$production))[SIAP$production]
SIAP$yield <- as.numeric(levels(SIAP$yield))[SIAP$yield]
SIAP$pmr <- as.numeric(levels(SIAP$pmr))[SIAP$pmr]
SIAP$value <- as.numeric(levels(SIAP$value))[SIAP$value]

write.csv(SIAP, file = "SIAP.csv")

str(SIAP)

#New variable: crop gruoup (items aggregated)
#cereals <- c("cebada", "alpiste", "amaranto", "maiz", "mijo",
            # "avena", "arroz", "centeno", "sorgo", "triticale", "trigo")
#cereals <- filter(SIAP, crop == cereals)



cereals <- filter(SIAP, crop == "cebada" | 
                       crop == "alpiste" |
                       crop == "amaranto" |
                       crop == "maiz" |
                       crop == "mijo" |
                       crop == "avena" |
                       crop == "arroz" |
                       crop == "centeno" |
                       crop == "sorgo" |
                       crop == "triticale" |
                       crop == "trigo")

cereals <- mutate(cereals, area = production/yield)

write.csv(cereals, file = "cereals.csv")


roots_tubers <- c("yuca", "papa", "barbasco", "betabel", "camote", "macal", "malanga")
roots_tubers <- filter(SIAP, crop == roots_tubers)
write.csv(roots_tubers, file = "roots_tuber")


#oil_crops <- c("higuerilla", "coco", "copra", "algodon", "cacahuates", "jojoba",
               #"linaza", "mostaza", "palma africana", "aceituna", "cartamo", "ajonjoli",
               #"soya", "girasol")
#oil_crops <- filter(SIAP, crop == oil_crops)

oil_crops <- filter(SIAP, crop == "higuerilla" | 
                    crop == "coco" |
                    crop == "copra" |
                    crop == "algodon" |
                    crop == "cacahuates" |
                    crop == "jojoba" |
                    crop == "arroz" |
                    crop == "linaza" |
                    crop == "mostaza" |
                    crop == "palma africana" |
                    crop == "aceituna" |
                      crop == "aceituna" |
                      crop == "cartamo" |
                      crop == "ajonjoli" |
                      crop == "soya" |
                      crop == "girasol")


#START RUNNING HERE
SIAP <- read.csv("SIAP.csv")
SIAP <- as.tibble(SIAP) 
SIAP %>% 
  DT::datatable()


# Same spatial resolution
# From 1980 to 2002 the data is at the state level.
# From 2003 to 2016 the data is at the municipal level.

# Transform the resolution to state level.

#SIAP_1980_2002 <- 
  
SIAP %>%
  filter(ag_year < "2003") %>% 
  count(mun)

SIAP %>% 
  filter(ag_year > "2002") %>% 
  count(mun)

SIAP %>% 
  filter(ag_year > "2002") %>% 
  group_by(state) %>% 
  cumsum(production)


SIAP %>%
  filter(ag_year > "2002", state == "aguascalientes") %>%
  select(ag_year, mun, production) %>%
  group_by(ag_year) %>%
  filter(min_rank(desc(production)) < 2 | min_rank(production) < 2) %>% 
  arrange(ag_year) %>%
  print(n = Inf)

asia <- SIAP %>%
  filter(ag_year > "2002") %>%
  select(ag_year, state, production) %>%
  group_by(ag_year)

SIAP_1980_2002 <- filter(SIAP, ag_year < 2003)
write.csv(SIAP_1980_2002, file = "SIAP_1980_2002.csv")
SIAP_2003_2016 <- filter(SIAP, ag_year > 2002)
write.csv(SIAP_2003_2016, file = "SIAP_2003_2016.csv")

SIAP_2003_2016 %>% group_by(state) %>% summarise(cumsum())

#Tidyverse


hist(SIAP$ag_year)

barplot(table(SIAP$state))

p <- ggplot(filter(SIAP, state != "aguascalientes"),
            aes(x = production, y = yield)) # just initializes
p <- p + scale_x_log10() # log the x axis the right way
p + geom_point() # scatterplot
p + geom_point(aes(color = state)) # map continent to color
p + geom_point(alpha = (1/3), size = 3) + geom_smooth(lwd = 3, se = FALSE)
#> `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
p + geom_point(alpha = (1/3), size = 3) + facet_wrap(~ state) +
  geom_smooth(lwd = 1.5, se = FALSE)
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'


#### Categorical ####
ggplot(data = SIAP) +
  geom_bar(mapping = aes(x = cycle))

ggplot(data = SIAP) +
  geom_bar(mapping = aes(x = water))

ggplot(data = SIAP) +
  geom_bar(mapping = aes(x = type))

#### Continous ####
ggplot(data = SIAP) +
  geom_histogram(mapping = aes(x = ag_year), binwidth = 0.5)

food_crops <- SIAP %>% 
  filter(type == "food")

perennial <-  food_crops %>% 
  filter(cycle == "perennial")

chiapas <- perennial %>% 
  filter(state == "chiapas")

ggplot(chiapas, aes(x = production, colour = water)) +
  geom_freqpoly(bindwidth = 0.01)

pairs(chiapas[11:15], main = "Chiapas",
      pch = 21, bg = c("red", "green3", "blue", "yellow", "pink")[unclass(chiapas$crop)])

pairs(chiapas[12:14], main = "Chiapas",
      pch = 21, bg = c("red", "green", "blue")[unclass(chiapas$crop)])

aguacate_chis <- chiapas %>% 
  filter(crop == "aguacate")
pairs(aguacate_chis[12:14], main = "Chiapas",
      pch = 21, bg = c("red", "blue")[unclass(aguacate_chis$water)])

pairs(perennial[12:14], main = "Perennial",
      pch = 21, bg = c("red", "blue")[unclass(perennial$water)])

#ggplot(chiapas, aes(ag_year, production)) +
  #geom_line()




food_crops %>% 
  group_by(crop)

ggplot(data = perennial, mapping = aes(x = production)) +
  geom_histogram(binwidth = 0.6)

ggplot(data = perennial, mapping = aes(x = production, colour = crop)) +
  geom_freqpoly(bindwidth = 0.1)

ggplot(data = food_crops, mapping = aes(x = production, colour = cycle)) +
  geom_freqpoly(bindwidth = 0.01)

ggplot(data = food_crops, mapping = aes(x = yield, colour = cycle)) +
  geom_freqpoly(bindwidth = 0.01)

ggplot(data = food_crops, mapping = aes(x = harvested, colour = cycle)) +
  geom_freqpoly(bindwidth = 0.01)

ggplot(data = food_crops, mapping = aes(x = production)) +
  geom_histogram(binwidth = 0.5)



set.seed(42)
x <- rnorm(1000)
hist(x,breaks="FD")

library(ggplot2)
breaks <- pretty(range(food_crops$production), n = nclass.FD(food_crops$production), min.n = 1)
bwidth <- breaks[5]-breaks[1]
#df <- data.frame(x)
ggplot(food_crops,aes(food_crops$production))+geom_histogram(binwidth=bwidth,fill="white",colour="black")


# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)

# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)
data <- perennial %>% filter(ag_year== 2007) %>% dplyr::select(-ag_year)
# Most basic bubble plot
data %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, fill=continent)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 24), name="Population (M)") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  #theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Life Expectancy") +
  xlab("Gdp per Capita") +
  theme(legend.position = "none")

chiapas %>% 
  arrange(desc(harvested)) %>% 
  mutate(crop = factor(crop)) %>% 
  ggplot(aes(x=yield, y=production, size=harvested, fill=state)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 24), name="Population (M)") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  #theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Life Expectancy") +
  xlab("Gdp per Capita") +
  theme(legend.position = "none")


#Create data
a <- seq(1,29)+4*runif(29,0.4)
b <- seq(1,29)^2+runif(29,0.98)

# I divide the screen in 2 line and 1 column only
my_screen_step1 <- split.screen(c(2, 1))

# I add one graph on the screen number 1 which is on top :
screen(my_screen_step1[1])
plot( a,b , pch=20 , xlab="value of a" , cex=3 , col=rgb(0.4,0.9,0.8,0.5) )


# I divide the second screen in 2 columns :
my_screen_step2 <- split.screen(c(1, 2), screen = my_screen_step1[2])
screen(my_screen_step2[1])
hist(a, border=F , col=rgb(0.2,0.2,0.8,0.7) , main="" , xlab="distribution of a")
screen(my_screen_step2[2])
hist(b, border=F , col=rgb(0.8,0.2,0.8,0.7) , main="" ,  xlab="distribution of b")

#### Simple Fast Exploratory Data Analysis in R with DataExplorer Package ####
# From https://towardsdatascience.com/simple-fast-exploratory-data-analysis-in-r-with-dataexplorer-package-e055348d9619

#Install if the package doesn't exist 
install.packages('DataExplorer') 
library(DataExplorer)

choco = read.csv('SIAP.csv', header = T, stringsAsFactors = F)
plot_str(choco)
plot_missing(choco)
plot_histogram(choco)
plot_density(choco)
plot_correlation(choco, type = 'continuous','Production')
plot_bar(choco)
create_report(choco)



dplyr::group_by(choco, state) %>% 
  DT::datatable()


by_vs_am <- choco %>% group_by(state, crop)
by_vs <- by_vs_am %>% summarise(sum(production))
by_vs
by_vs %>% summarise(n = sum(n))
