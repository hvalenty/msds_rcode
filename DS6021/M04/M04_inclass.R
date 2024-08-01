# 7/18/24

ct <- read.csv('data/Clinical_trial.csv')
anova <- aov(Pain_Rating~Drug, data=ct)
summary(anova)
# at least one of the mean pain ratings are different

TukeyHSD(anova, conf.level = 0.95)
plot(TukeyHSD(anova, conf.level = 0.95))
# We have enough statistical evidence to conclude that mean
# pain rating for drug A is much lower then drugs B and C.

# Can say below because it is an experiment
# Drug A is more effective in treating migraine headaches compared to drug B and C.

# We are 95% confident that the mean pain rating for 
# drug A is much lower then drug C