library(dplyr)
library(ggplot2)

df <- read.csv('ClassData.csv')

df <- df %>%
  mutate(Slp=as.numeric(Sleep_Hrs))

t.test(df$Slp)

#Bootstrapping
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

prop.test(40, 100, p=0.85, alternative = 'l')
# p-value basically zero < 0.01
# p-value less than level of significance = REJECT the Null
# statistically significant

#default is two sided test
t.test(df$Slp, mu=6, alternative = 'g')

prop.test(c(7,15), c(15,19))
