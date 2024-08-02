# 07/26/24

# data
n=100
b0=5
b1=1

x = rnorm(n, mean=20, sd=4)
x
y = b0 + b1*x + rnorm(n, mean=0, sd=6)
y

# plot
plot(x, y, type='n')
points(x, y, col='black', pch=20)

mod1 <- lm(y~x)
coef(mod1)

abline(a=b0, b=b1, lty=1, lwd=2, col='black')
abline(lm(y~x),lty=2,lwd=2, col='red')
# incrase sample size get lm to fit to black line avg
# orediction model never perfect
# predictiong avg for someone with those features, but there will be some variance

# MSE 
yhat = coef(mod1)[1] + coef(mod1)[2]*x
mse = sum((y-yhat)^2)/length(y)

find.mse = function(y, x, a, b){
  yhat = a + b*x
  mse = sum((y-yhat)^2)/(length(y)-2)
  return(mse)
}
find.mse(y=y, x=x, a=coef(mod1)[1], b=coef(mod1)[2])
find.mse(y=y, x=x, a=coef(mod1)[1]+0.5, b=coef(mod1)[2]+0.5)

# simple start
a=seq(-20, 20, 1)
b=seq(-20, 20, 1)
b
#what pair of intercept and slope give the smallest

object.plain = matrix(NA, nrow=length(a), ncol=length(b))
object.ridge = matrix(NA, nrow=length(a), ncol=length(b))
object.lasso = matrix(NA, nrow=length(a), ncol=length(b))

for (i in 1:length(a)) {
  for (j in 1:length(b)) {
    object.plain[i,j] = find.mse(y=y, x=x, a=a[i], b=b[j]) 
    object.ridge[i,j] = find.mse(y=y, x=x, a=a[i], b=b[j]) + 0.2*(a[i]^2 + b[j]^2)
    object.lasso[i,j] = find.mse(y=y, x=x, a=a[i], b=b[j]) + 0.2*(abs(a[i]) + abs(b[j]))
    if((i %% 50==0) & (j==1)) {cat(paste0("iteration: ", i, "\n"))}
  }
}
object.plain
min(object.plain)
soln.plain=which(object.plain == min(object.plain), arr.ind = TRUE)
soln.plain
a[soln.plain[1]] ; b[soln.plain[2]]
# ridge has more penalty !
# lasso even more with abs.

#### Updated Plot
# look at it!
plot(x, y, type="n")
points(x, y, col="black", pch=20)
abline(a=b0,b=b1,lty=1,lwd=2, col="red")

abline(lm(y~x),lty=2, lwd=2, col="black")
abline(a=a[soln.plain[1]],b=b[soln.plain[2]], lty=1, lwd=2, col="purple")
abline(a=a[soln.ridge[1]],b=b[soln.ridge[2]], lty=1, lwd=2, col="blue")
abline(a=a[soln.lasso[1]],b=b[soln.lasso[2]], lty=1, lwd=2, col="forestgreen")


#Now Regreg

install.packages('glmnet')
install.packages('corrplot')
install.packages('lars')

library(glmnet)
library(corrplot)
library(lars)

# use penalties to make parameter selection
