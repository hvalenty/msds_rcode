# 07/17/24
df <- read.csv('nba.csv')
View(df)

table(df$W.L, df$home_away)
#CI below: (away - home, successes), sample size
prop.test(c(518,712), c(1230,1230))

#we are 95% confident that the winning proportion of all NBA games
# played at home is between 0.12 and 0.19 higher than when 
# the games are played away

# "all NBA games" need to refer to whole population 
#point of inference is take the leap into whole population

library(ggplot2)

ggplot(df, aes(x=home_away, fill=W.L))+
  geom_bar(position='fill')

t.test(PTS~home_away, data=df)
#use t.test for numerical variable (points)
# use prop.test (proportion) for categorical variable (W/L)

# we are 95% confident that the average number of points scored
# in all NBA games is between 1.16 and 3.05 points higher than
# away games.

ggplot(df, aes(x=home_away, y=PTS, fill=home_away))+
  geom_boxplot(outlier.color='green')+
  geom_jitter()

cl <- read.csv('classData.csv')
View(cl)

cl2 <- mutate(cl, 
              Age_Diff=as.numeric(gsub('years','', Age_Diff_Parents)))
#cl2 <- cl2[41, 17]

t.test(cl2$Age_Diff)

ct <- read.csv('Clinical_trial.csv')

ggplot(ct, aes(x=Drug, y=Pain_Rating, fill=Drug))+
  geom_violin()+
  geom_jitter()

ggplot(ct, aes(x=Drug, y=Pain_Rating, fill=Drug))+
  geom_boxplot()+
  geom_jitter()

anova <- aov(Pain_Rating~Drug, data=ct)
summary(anova)
