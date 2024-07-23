# 7/19/24

st <- read.csv('Startups.csv')
library(ggplot2)
ggplot(st, aes(x=R.D.Spend, y=Profit))+
  geom_point()+
  geom_smooth(method = 'lm')

cor(st$Profit, st$R.D.Spend)
sd(st$Profit)
sd(st$R.D.Spend)

b1 <- cor(st$Profit, st$R.D.Spend)*sd(st$Profit) / sd(st$R.D.Spend)
mean(st$Profit)-b1*mean(st$R.D.Spend)

mod <- lm(Profit~R.D.Spend, data=st)
coef(mod)
