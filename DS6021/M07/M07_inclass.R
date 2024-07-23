# 7/23/24
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
