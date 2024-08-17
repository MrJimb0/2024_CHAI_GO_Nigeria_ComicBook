#Graphing File
#Authors: Nicole Kim, BS and James Dickerson, MD MS 
options(scipen=999)

library(readxl)
library(tidyverse)
library(stargazer)
library(car)
library(lmerTest)
library(performance)
library(knitr)
library(broom)
library(ggplot2)

setwd("/Users/jamesdickerson/Library/CloudStorage/Box-Box/Dickerson Lab/Dickerson_Lab_Github/2024_CHAI_GO_Nigeria_ComicBook/Data_/Updated files from Nicole 8:17:24")
#setwd("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook")

df_total <- read_xlsx("df_total_cleaned.xlsx")
df_pre <- read_xlsx("df_pre_cleaned.xlsx")
df_post <- read_xlsx("df_post_cleaned.xlsx")
vax_numbers_long <- read_xlsx("vax_numbers_long.xlsx")
df_condensed <- read_xlsx("df_condensed.xlsx")

#Pre-test score by state
pre_post_test_scores_by_state %>%
  filter(Time == "Pre-test") %>%
  ggplot(aes(x = State, y = Mean)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymax = Mean + SD, ymin = Mean - SD), width = 0.2) +
  geom_text(aes(label = paste("n =", N)), vjust = -1, size = 3) +
  labs(title = "Pre-test Scores by State", x = "State", y = "Mean Score") +
  theme_classic()

#Then, we look at age, within each state
#Not a clear pattern. Note kaduna has no 10 year olds
pre_post_test_scores_by_state_age %>%
  filter(Time == "Pre-test") %>%
ggplot(aes(x = Age, y = Mean, color = State)) +
  geom_point() +
  geom_line() +
  labs(title = "Pre-test Scores by State and Age", x = "Age", y = "Mean Score") +
  theme_classic() +
  theme(legend.position = "bottom")

#By school
pre_post_test_scores_by_state_school %>%
  filter(Time == "Pre-test") %>%
  ggplot(aes(x = School, y = Mean, color = State)) +
  geom_point() +
  geom_line() +
  labs(title = "Pre-test Scores by State and School", x = "School", y = "Mean Score") +
  theme_classic() +
  theme(legend.position = "bottom")

#Vaccination rates 
#This is absolute numbers
ggplot(vax_numbers_long, aes(x = state, y = vaccinations, fill = time)) +
  geom_bar(stat = "identity") +
  labs(title = "Vaccination Numbers by State", x = "State", y = "Number of Vaccinations") +
  theme_classic() +
  theme(legend.position = "bottom")

#Plots used for the modeling 

#As expected, there is a positive linear relationship between pre and post test score
ggplot(df_condensed, aes(x = mean_score_pre, y = mean_score_post)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + 
  labs(title = "Pre-test vs Post-test Mean Scores", x = "Pre-test Mean Score", y = "Post-test Mean Score")

#Plot them by state. Suggests that we should be using random intercepts, as expected
ggplot(df_condensed, aes(x = mean_score_pre, y = mean_score_post, color = State)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + 
  labs(title = "Relationship Between Pre-test and Post-test Mean Scores by State",
       subtitle = "Color-coded by State",
       x = "Pre-test Mean Score",
       y = "Post-test Mean Score")

# Change the title and labels to accurately reflect the x-axis variable
ggplot(df_condensed, aes(x = mean_score_post, y = change_vaccination_status, color = State)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + 
  labs(title = "Relationship Between Post-test Mean Scores and Change in Vaccination Status by State",
       subtitle = "Color-coded by State",
       x = "Post-test Mean Score",
       y = "Change in Vaccination Status (%)")

# Change the title and labels to accurately reflect the x-axis variable
ggplot(df_condensed, aes(x = change_score, y = change_vaccination_status)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + 
  labs(title = "Relationship Between Change in Mean Scores and Change in Vaccination Status",
       x = "Change in Mean Score",
       y = "Change in Vaccination Status (%)")

# Change the title and labels to accurately reflect the x-axis variable
ggplot(df_condensed, aes(x = change_score, y = change_vaccination_status, color = State)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + 
  labs(title = "Relationship Between Change in Mean Scores and Change in Vaccination Status by State",
       subtitle = "Color-coded by State",
       x = "Change in Mean Score",
       y = "Change in Vaccination Status (%)")
