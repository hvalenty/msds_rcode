# 07/30/24

# categorical predictors
## variables which are not numeric
## dummy variables: codifies variables with zeros and ones (binary)
setwd('C:/Users/Valenty/msds_rcode/DS6021')

df <- read.csv('data/insurance.csv')
head(df)
mod1 <- lm(charges~smoker, data=df)
summary(mod1)

# indicator function: 
# (intercept) avg charges for people who dont smoke
# smokeryes -- difference between people who smoke and do not average charges 

# could also use this for t test with two pops.
t.test(charges~smoker, data=df)

#predict for smokers only
df2 <- data.frame(smoker='yes')
predict(mod1, df2)

#predict based on region
mod2 <- lm(charges~region, data=df)
summary(mod2)

# (intercept) estimate -- northeast 
# on avg charges for NW is -988.8 less than charges for the northeast
# all relative to the reference variable which is determined default alphabetically !!
# simple linear regression in terms of categorical predictors

#changes levels with factors
df$region2 <- factor(df$region, level = c('southeast','northeast','northwest', 'southwest'))
head(df)

mod3 <- lm(charges~region2, data=df)
summary(mod3)
# changing the order changes the p-values -- reference make difference in values larger or smaller

# could also do ANOVA for the different regions
anov <- aov(charges~region, data=df)
summary(anov)
TukeyHSD(anov)

#look at smokers again with age
library(ggplot2)
ggplot(df, aes(x=age, y=charges))+
  geom_jitter()+
  geom_smooth(method='lm', se=F)

#smoker vs. non -- with separate fit lines by smoker group
ggplot(df, aes(x=age, y=charges, colour = smoker))+
  geom_jitter()+
  geom_smooth(method='lm', aes(group=smoker), se=F)

mod4 <- lm(charges~age+smoker, data=df)
summary(mod4)
# (intercept) estimate -- non-smoker aged zero
# age Estimate -- non-smokers pay 274.87 more for every year they age
# smokeryes Estimate -- smokers pay on avg 23855.30 more than non smokers at age zero

# * for interaction with variables
mod5 <- lm(charges~age*smoker, data=df)
summary(mod5)
# age:smokeryes -- every change in age for smokers is more on avg by 267.25 + 37.99

# mod4 no interaction, mod5 interaction
ggplot(df, aes(x=age, y=charges, colour = smoker))+
  geom_jitter()+
  geom_smooth(method='lm', model.extract(mod5), se=F)

# make a prediction
df3 <- data.frame(age=26, smoker='no')
predict(mod5, df3, interval = 'prediction')

df4 <- df[,-8]
head(df4)
mod6 <- lm(charges~., data=df4)
summary(mod6)

# residual se -- range error the model might be off

library(broom)
library(dplyr)
dat_ins <- mod5 %>% augment(df4)
View(dat_ins)

filter(dat_ins, .std.resid>2)
