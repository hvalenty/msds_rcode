# 8/2/24

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
kcvglmnet$lambda.1se

predict(rmod, type='response', s=kcvglmnet$lambda.min, newx=X[1:2,])
# try alpha = 1 for lasso
#predict values of coefficients
predict(rmod, type='coefficient', s=kcvglmnet$lambda.min, newx=X[1:2,])

plot(rmod, label=T, xvar='lambda')+abline(v=log(kcvglmnet$lambda.1se))

head(la2)
# PCA
pca <- princomp(la2, fix_sign = T) # sign of loading more consistent
summary(pca)
# tells in terms of variability
# PC1 captures 77% of variability
# PC2 16%...
plot(pca, type='l')
# scaled variances
pca2 <- princomp(la2, cor=T, fix_sign = T) 
summary(pca2)
pca2$loadings
# bring the actual observations in data rep in PCA
# OG data set once its been transformed by PCA
pca2$scores
# vectors -- magnitude is value PC, and direction points to which
# component it contributes to 
# if variables bunched they are similar
# numbers in plot correspond to row numbers
biplot(pca2)
score <- data.frame(pca2$scores)
# data frame same but converted to PC values using linear combination

la3 <- la2[,-1]
pca3 <- princomp(la3, cor=T, fix_sign = T)
summary(pca3)
# could do bi-plot here
# use scores as predictors of points
pcadata <- data.frame(Points=la2$Points, pca3$scores)
pcareg <- lm(Points~., data = pcadata)
summary(pcareg)
car::vif(pcareg)
# may be too perfect, remove some columns
pcareg2 <- lm(Points~Comp.1+Comp.2+Comp.3, data=pcadata)
summary(pcareg2)
car::vif(pcareg2)
# all ones means no multicollinearity
# eigenvectors are orthogonal

# could continue with this process but needs to reconvert components to OG data
install.packages('pls')
library(pls)
la4 <- subset(la2, select = -c(Wins, Draws, Loses))
# PCR same argu as lm
pcareg3 <- pcr(Points~., data=la4, scale = T)
summary(pcareg3)
# X variability explained
# points variability in points -- compared to r-squared
# both are cumulative

newda <- la4[c(1,2),]
predict(pcareg3, newdata = newda, ncomp = 3)
# compare to OG points rows
predict(pcareg3, newdata = newda, ncomp = 8)
