library(tidyverse)
library(broom)
setwd('C:/Users/Valenty/msds_rcode/DS6021')
ins <- read.csv('data/insurance.csv')
mod1 <- lm(charges~., data=ins)
summary(mod1)

# calculate statistical values to test for outliers
diag <- mod1 %>%
  augment(data=ins)
head(diag)
# for abs that meet the abs > 3, mark it an outlier
# 0.997 fall within 3 st deviations 
out <- filter(diag, abs(.std.resid) > 3)
out
# if hat > 2*(# of predictors) -- leverage
lev <- filter(diag, .hat > 2*(6+1)/nrow(ins))
# -- influential
infl <- filter(diag, .cooksd > 4/nrow(ins))
infl

# train test split -- sample does not replace by default
split <- sample(1:nrow(ins), size = floor(0.8*nrow(ins)))
split

#select training data from OG df
train_data <- ins[split,]
#select testing (other 20% of OG df)
test_data <- ins[-split,]
names(ins)
# model without sex (not significant)
mod2 <- lm(charges~age+bmi+children+smoker+region, data=train_data)
summary(mod2)

predictions <- predict(mod2, test_data)
rmse <- sqrt(mean((predictions - test_data$charges)^2))
rmse

install.packages('caret')
library(caret)
RMSE(predictions, test_data$charges)
R2(predictions, test_data$charges) # adjusted r2
MAE(predictions, test_data$charges)

#take every fold and repeat the process
map_dbl(1:5,~.x^2)
CV <- function(data,k){
    folds <- createFolds(ins$charges,k=k)
    map_dbl(folds, function(indicies){
      train_data <- data[-indicies,]
      test_data <- data[indicies,]
      
      mod5 <- lm(charges~.-sex, data = train_data)
      predictions <- predict(mod5, test_data)
      sqrt(mean((predictions-test_data$charges)^2))
      }) %>% mean()
}

CV(ins, 5)
# 5 folds 10 times -- captures more variability when repeated
repeated <- replicate(10, CV(ins, 5))
mean(repeated)
