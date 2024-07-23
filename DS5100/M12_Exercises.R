library(vctrs)
library(tidyverse)

head(mpg)

#Task 1
mpg %>% ggplot()
#Task 2
mpg %>%
  ggplot(mapping = aes(x=cyl, y=hwy)) +
           geom_point()

#Task 3
mpg %>%
  ggplot(mapping = aes(x=drv, y=class)) +
  geom_point()

#Task 4
x <- seq(-6*pi, 6*pi, length.out = 100)
x_df <- data.frame(x=x, y=sin(x)/x)
x_df %>%
  ggplot() +
  geom_line(aes(x=x, y=y))

#Task 5
cars %>%
  ggplot(mapping = aes(x=speed, y=dist)) +
  geom_point()

#Task 6
cars %>%
  ggplot(mapping = aes(x=speed, y=dist, color=dist > 80)) +
  geom_point()

#Task 7
help("scale_color_manual")
cars %>%
  ggplot(mapping = aes(x=speed, y=dist, color=dist > 80)) +
  scale_color_manual(values = c('blue','red')) +
  geom_point()

#Task 8
cars %>%
  ggplot(mapping = aes(x=speed, y=dist, color=dist > 80)) +
  scale_color_manual(values = c('blue','red')) +
  geom_point() +
  geom_smooth(method='lm')

#Task 9
cars %>%
  ggplot(aes(speed)) +
  geom_histogram() 

cars %>%
  ggplot(aes(dist)) +
  geom_histogram()

#Task 10
mpg %>%
  ggplot(mapping = aes(hwy, cty)) +
  geom_point() +
  facet_grid(drv ~ cyl)

#Task 11
iris %>%
  ggplot(aes(x=Sepal.Length, y=Sepal.Width, color=Species, shape=Species)) +
  geom_point() +
  geom_density2d() +
  ggtitle('IRIS') +
  theme_light()
