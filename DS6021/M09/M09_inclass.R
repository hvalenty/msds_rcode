# 07/25/24

st <- read.csv('data/Startups.csv')

mod1 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend, data = st)
summary(mod1)
# keep R.D.Spend
# remove Administration > 0.05 - not a good predictor of profit
# make call on Marketing.Spend to remove


mod2 <- lm(Profit~R.D.Spend+Marketing.Spend, data=st)
summary(mod2)
# estimates smaller when remove 

# make alpha smaller or p-value bigger !! 
#  With 2 predictors: 
#     1. divide alpha by 2 and compare true p-values
#     2. multiply p-values by 2 and compare OG alpha

library(MASS)

aic <- stepAIC(mod1, direction='both')
summary(aic)

library(car)
vif(aic)

# if using AIC, then follow with VIF to check picks

avPlots(mod1)
