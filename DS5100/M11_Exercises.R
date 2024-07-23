#Task 11.1
library(tidyverse)

#Task 11.2
scores <- 
  tibble(
    name = c("mike", "carol", "greg", "marcia", "peter", "jan", "bobby", "cindy", "alice"),
    school = c("south", "south", "south", "south", "north", "north", "north", "south", "south"),
    teacher = c("johnson", "johnson", "johnson", "johnson",  "smith", "smith", "smith", "perry", "perry"),
    sex = c("male", "female", "male", "female", "male", "female", "male", "female", "female"),
    math_score = c(4, 3, 2, 4, 3, 4, 5, 4, 5),
    reading_score = c(1, 5, 2, 4, 5, 4, 1, 5, 4)
  )

#Task 11.3
scores

#Task 11.4
help(slice)
scores %>%
  slice(1:3)

top3 <- scores %>%
  slice(1:3)

#Task 11.5
help(arrange)
scores %>%
  arrange(desc(math_scores))

#Task 11.5
scores %>%
  arrange(name)

#Task 11.7
scores %>%
  arrange(sex)

#Task 11.8
scores %>%
  arrange(school) %>%
  arrange(teacher) %>%
  arrange(sex) %>%
  arrange(math_score) %>%
  arrange(reading_score)
#OR
scores %>%
  arrange(school, teacher, sex, math_score, reading_score)

#Task 11.9
scores %>%
  select(name, math_score, reading_score)

#Task 11.10
scores %>%
  select(-sex)

#Task 11.11
scores %>%
  select(-sex, -reading_score)

#Task 11.12
scores %>%
  select(sex, everything())

#Task 11.13
help(filter)
scores %>%
  filter(sex == 'male', school == 'south')

#Task 11.14
scores %>%
  filter(math_score > mean(math_score))

#Task 11.15
scores %>%
  filter(math_score >= 4, reading_score >= 3)

#Task 11.16
scores %>%
  filter(math_score <= 3 | reading_score <=3)

#Task 11.17
scores %>%
  filter (reading_score != 1 & reading_score != 5)
#OR
scores %>%
  filter(reading_score %in% 2:4)

#Task 11.18
scores %>%
  filter(starts_with('m'))
  
#Task 11.19
scores %>%
  group_by(teacher) %>%
  filter(max(math_score) == 5)

#Task 11.20
scores %>%
  group_by(sex) %>%
  filter(mean(math_score) == 4)

#Task 11.21
scores %>%
  mutate(math_score*10, reading_score*12)

#Task 11.22
scores %>%
  mutate((math_score + reading_score)/2)

#Task 11.26
scores %>%
  group_by(sex) %>%
  mutate(math_score_centered_by_sex = mean(math_score) - math_score) %>%
  arrange(desc(math_score_centered_by_sex))

#Task 11.27
scores %>%
  group_by(teacher) %>%
  mutate(reading_score_centered_by_teacher = mean(reading_score) - reading_score) %>%
  arrange(desc(reading_score_centered_by_teacher))

#Task 11.32
scores %>%
  group_by(school) %>%
  summarize(min(math_score))

#Task 11.33
scores %>%
  group_by(teacher) %>%
  summarize(max(math_score))
