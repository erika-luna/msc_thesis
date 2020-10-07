#### Descriptive Statistics ####
summary(SIAP) 

hist(SIAP$planted)


#t.test(DV ~ IV, data =, var.eq = )
t.test(scores$SATV ~ scores$gender, data = scores, var.eq = T)
t.test(SIAP$planted ~ SIAP$water, data = SIAP, var.eq = T)

#anova aov(DV ~ IV, data = )
m1 <- aov(scores$SATV ~ scores$education, data = scores)
m1 <- aov(SIAP$planted ~ SIAP$cycle, data = SIAP)

boxplot(SIAP$planted ~ SIAP$cycle)

#Pearson correlation
#For this test, in contrast to t-test and ANOVA, we don't have to sppecify the ind and the dpndt variables.
#Runs from -1 to 1. 0 is no correlation, neg value is a neg correlation.
cor.test(scores$SATV, scores$SATQ)
#scatterplot
plot(x,y)
abline(lm(scores$SATQ, scores$SATV)) #to add the regression line

#Simple linear regression
m2 <- lm(scores$SATV ~ scores$ACT)
summary(m2)



 

