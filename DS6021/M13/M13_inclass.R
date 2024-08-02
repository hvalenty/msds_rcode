# 8/1/24

setwd('C:/Users/Valenty/msds_rcode/DS6021')
ins <- read.csv('data/insurance.csv')
mod0 <- lm(charges~.-sex, data=ins)
summary(mod0)

# cross validation
library(caret)
# use other modeling function -- can build any function specify with model
mod1 <- train(charges~.-sex, method='lm', data=ins)
summary(mod1)
# by default will do k fold (below is 5 fold CV)
control <- trainControl(method = 'cv', number = 5)

mod2 <- train(charges~.-sex, method='lm', trControl=control, data=ins)
summary(mod2)

controlrep <- trainControl(method = 'repeatedcv', number = 5, repeats = 10)
controlloocv <- trainControl(method = 'LOOCV')
# CV is to test how you will do in a new dataset -- RMSE or MAE are of interest
# CV sees how the model is doing/diagnose issues
mod2$results$RMSE

# la liga -- lots of columns
la <- read.csv('data/laliga.csv')
la2 <- la[,-c(1,3,9)]
# make a lil model
mod_la <- lm(Points~., data=la2)
summary(mod_la)
aic <- MASS::stepAIC(mod_la, direction='both', Trace=FALSE)
summary(aic)

car::vif(aic$mod_la)


# Model steps
##  look at all/desired variables using lm()
##  use step AIC to pick parameters
##  but sometimes step AIC is not good...
##  use VIF for multicollinearity
##  

library(glmnet)
# design matrix without 0+ would include all the ones
# need to remove for glmnet
X <- model.matrix(Points~0+., data=la2)
X
y <- la2$Points
# alpha picks either squared penalty or abs penalty
rmod <- glmnet(x=X,y=y, alpha=0) # ridge
# look at model and coeff based on diff values of lambda
plot(rmod, label=T, xvar='lambda')
# tiny numbers are col numbers
# coefficients on zero line at x-value are not in model

# deviance ~ adj r-squared
plot(rmod, label=T, xvar='dev')
# cols 6 and 7 good because far away (?)
length(rmod$lambda) # number of lambdas looking at
coef(rmod)[,50] # 50th lambda

# pick lambda based on CV
# randomly run on minimums -- but when rerun the min can change
# pick 1 sd. away from a minimum shown (principle of parsimony)
kcvglmnet <- cv.glmnet(x=X, y=y, alpha=0, nfolds=3)
kcvglmnet$lambda.min

predict(rmod, type='response', s=kcvglmnet$lambda.min, newx=X[1:2,])
# try alpha = 1 for lasso