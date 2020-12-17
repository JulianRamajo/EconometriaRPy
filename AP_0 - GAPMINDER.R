library(tidyverse)
gapminder <- read_delim("GAPMINDER.csv", ";", escape_double = FALSE, trim_ws = TRUE)
# GM <- read_csv("https://raw.githubusercontent.com/CerebralMastication/r_for_the_student/master/01_data/gapminder.csv")
gapminder
#
library(skimr)
skim(gapminder)
#
# Operaciones básicas del Tidyverse
#
# dplyr verbs
#
# select
#
gapminder_selected <- select(gapminder, year, country, pop, gdpPercap)
#
# filter
#
gapminder_filtered <- filter(gapminder_selected, year>=1980)
#
# mutate
#
gapminder_mutated <- mutate(gapminder_filtered, GDP=gdpPercap*pop)
#
# group_by
#
gapminder_grouped <- group_by(gapminder_mutated, country)
#
# summarise
#
gapminder_summarised <- summarise(gapminder_grouped, AVG_GDP=mean(GDP))
#
# arrange
#
gapminder_arranged_ascending <- arrange(gapminder_summarised, AVG_GDP)
gapminder_arranged_descending <- arrange(gapminder_summarised, -AVG_GDP)
#
# The pipe operator (%>%)
#
AVG_GDP <- 
  gapminder %>% 
  select(year, country, pop, gdpPercap) %>% 
  filter(year>=1980) %>% 
  mutate(GDP=gdpPercap*pop) %>% 
  group_by(country) %>% 
  summarise(AVG_GDP=mean(GDP)) %>% 
  arrange(-AVG_GDP)
#
# Plotting and EDA
#
# esquisse plots
# Select the 'ggplot2' builder in the Addins menu of RStudio or ...
#
esquisse:::esquisser()
#
# GGally technique
library(GGally)
#
gapminder %>% select(-country) %>% ggpairs()
#
# Regression
#
# Simple linear model
#
model_1 <- lm(lifeExp ~ gdpPercap, data=gapminder)
summary(model_1)
#
model_2 <- lm(lifeExp ~ gdpPercap + year, data=gapminder)
summary(model_2)
#
# LRM assumptions
#
library(lindia)
model_2 %>% 
  gg_diagnose(plot.all=TRUE,boxcox=TRUE)
#
model_3 <- lm(lifeExp ~ log(gdpPercap) + year, data=gapminder)
summary(model_3)
model_3 %>% 
  gg_diagnose(plot.all=TRUE,boxcox=TRUE)
#
