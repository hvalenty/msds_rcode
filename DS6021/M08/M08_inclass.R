# 07/24/24
df <- read.csv('data/Startups.csv')

mod0 <- lm(Profit~R.D.Spend, data=df)
coef(mod0)

df2 <- data.frame(R.D.Spend=c(165349.20, 500000))
# c(interpolation, extrapolation) within this example
predict(mod0, newdata=df2)

predict(mod0, newdata=df2, interval='prediction', level=0.95)
predict(mod0, newdata=df2, interval='confidence', level=0.95)

# Using data from yesterday
library(tidyverse)
df <- read.csv('data/Startups.csv')

long <- gather(df, key='predictor', value = 'value',
               R.D.Spend, Administration, Marketing.Spend)
ggplot(long, aes(x=value, y=Profit, color=predictor))+
  geom_point()+
  facet_wrap(~predictor, scales='free_x')

# predictors need plus
mod1 <- lm(Profit~R.D.Spend + Administration + Marketing.Spend,
           data=df)
coef(mod1)

df_pred <- mutate(df, predictions=fitted(mod1),
                  resid=residuals(mod1))
df_pred

ggplot(df_pred, aes(x=predictions, y=resid))+
  geom_point()+
  geom_hline(yintercept = 0, color='red')

# QQ plot: check if residual follow normal distribution
ggplot(df_pred, aes(sample=resid))+
  stat_qq()+
  stat_qq_line(color='green')

planet <- read.csv('data/PlanetsData.csv')

ggplot(planet, aes(x=distance, y=revolution))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)

mod2 <- lm(revolution~distance, data=planet)
coef(mod2) 
planet_pred <- mutate(planet, pred=fitted(mod2), resid=residuals(mod2))

ggplot(planet_pred, aes(x=pred, y=resid))+
  geom_point()+
  geom_hline(yintercept=0, color='purple')
# U-shaped error indicates possible quadratic fit better

ggplot(planet_pred, aes(sample=resid))+
  stat_qq()+
  stat_qq_line()

planet2 <- mutate(planet, log_dist=log(distance), log_rev=log(revolution))

ggplot(planet2, aes(x=log_dist, y=log_rev))+
  geom_point()

mod3 <- lm(log_rev~log_dist, data=planet2)
coef(mod3)

planet_pred2 <- mutate(planet2, pred=fitted(mod3), resid=residuals(mod3))
ggplot(planet_pred2, aes(x=pred, y=resid))+
  geom_point()+
  geom_hline(yintercept=0, color='orange')

#making a prediction with existing planet model
-0.9031235+1.5013054*log(93)
exp(5.901693)

new <- data.frame(log_dist=log(93))
predict(mod3, new)
predict(mod3, new, interval='prediction')

ggplot(df, aes(x=R.D.Spend, y=Profit))+
  geom_point()+
  geom_smooth(method='lm', se=F)
  #geom_hline(yintercept = Avg_profit)

summary(mod0)
sqrt(0.9465)
cor(Profit~R.D.Spend)

st <- df[,-4]
head(st)
cor_mat <- round(cor(st),2)
cor_mat

install.packages('ggcorrplot')
library(ggcorrplot)

ggcorrplot(cor_mat, lab=T, type='lower')

ggcorrplot(cor_mat, lab=T, type='lower', method='circle')

mod10 <- lm(Profit~R.D.Spend+Administration, data=df)
coef(mod10)
summary(mod10)

install.packages('car')
library(car)
vif(mod10)
