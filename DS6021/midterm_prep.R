# 07/28/24 -- Midterm Prep

library(tidyverse)
library(ggplot2)

# proportion test
prop.test(c(7,15), c(15,19))
prop.test(40, 100, p=0.85, alternative = 'l')

# t-test for means
df <- read.csv('data/ClassData.csv')
df <- df %>%
  mutate(Slp=as.numeric(Sleep_Hrs))
t.test(df$Slp)
# default is two sided test
t.test(df$Slp, mu=6, alternative = 'g')

# bootstrapping
mean_calc <- function(x){
  return(mean(x, na.rm = T))
}

boot_means <- replicate(10000, {
  boot_data <- sample(df$Slp, replace = TRUE)
  mean_calc(boot_data)
})
boot_df <- data.frame(boot_means)

ggplot(boot_df, aes(x=boot_means))+
  geom_density()

quantile(boot_means, c(0.025, 0.975))

