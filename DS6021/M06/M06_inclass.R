# 7/22/24
library(tidyverse)
df <- read.csv('data/Startups.csv')

ggplot(df, aes(x=R.D.Spend, y=Profit))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE, color='red')

model <- lm(Profit~R.D.Spend,data=df)
coef(model)

#bootstrapping

boot_estimate <- replicate(1000,{
boot_sam <- df[sample(1:nrow(df), nrow(df), 
                      replace=TRUE),]
boot_model <- lm(Profit~R.D.Spend, data=boot_sam)
coef(boot_model)
})
View(boot_estimate)
# y = Intercept + Slope*R.D
# y = 4.70e4 + 0.862*R.D

estm <- data.frame(t(boot_estimate))

ggplot(df, aes(x=R.D.Spend, y=Profit))+
  geom_point()+
  geom_abline(data = estm, aes(intercept = X.Intercept., slope = R.D.Spend), color='pink')+
  geom_smooth(method='lm', se=FALSE, color='blue')

summarize(estm, mean_b0=mean(X.Intercept.), mean_b1=mean(R.D.Spend))

# MLR
#Replace right side of ~ with a . and it grabs all columns in df
mod2 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend, data=df)
coef(mod2)


x<- cbind(1, df$R.D.Spend, df$Administration, df$Marketing.Spend)
x
xtx <- t(x)%*%x
xtx

inverse_xtx <- solve(xtx)
xty <- t(x)%*%df$Profit
beta <- inverse_xtx %*% xty
beta

df2 <- gather(df, key = 'predictor', value = 'value',
              R.D.Spend, Administration, Marketing.Spend)
df2
# will use facet wrap to make scatterplots for all predictors together