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
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  geom_errorbar(aes(ymax = Mean + SD, ymin = Mean - SD), width = 0.2) +
  geom_text(aes(label = paste("n =", N)), vjust = -1, size = 3) +
  labs(title = "Pre-test Scores by State", x = "State", y = "Mean Score") +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line( size=.2, color="gray80"),
        panel.background = element_blank()) 

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
  mutate(School = factor (School, levels = c("AMAC LGA_Aleyita Jnr Sec Schl", "Bwari LGA_Gov Jnr Sec Schl, Dut",
                                             "KD North_GGGS Kabala Costain", "Zaria LGA_Demonstration Sec Sch", 
                                             "Alimosho LGA_Idimu Junior High", "Mainland LGA_Wesley Girls",
                                             "Andoni LGA_UBE Agwut Obolo", "Andoni LGA_UBE Asarama", "ObioAkpor LGA_Comm Girls Sec Sc", "ObioAkpor LGA_Govt Girls Sec Sc"))
         ) %>%
  ggplot(aes(x = School, y = Mean, fill = State)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymax = Mean + SD, ymin = Mean - SD), width = 0.2) +
  labs(title = "Pre-test Scores by State and School", x = "School", y = "Mean Score") +
  theme_classic() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle=65, hjust=1))

#relationship between parent education & baseline scores 
df_edu <- read_xlsx("parent_edu_vs_pre_test_score.xlsx")

df_edu <- df_edu %>% mutate(edu_level = factor(edu_level, 
                                               levels=c("None", "SSCE", "Bachelor's degree", "Postgraduate")))
ggplot(data=df_edu, aes(x=parent, y= mean, fill = edu_level)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.9)) + 
  facet_grid(~State, switch = "x", scales ="free_x") +
  geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), width = 0.2, position = position_dodge(width = 0.9)) +
  labs(title = "Parent education level vs. Pre-test score", x = " ", y = "Mean Score") + 
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line( size=.2, color="gray80"),
        panel.background = element_blank(),
        legend.position = "bottom") + 
  scale_fill_manual(values=c("pink", "palevioletred1", "violetred1", "maroon"))

#Vaccination rates 
#This is absolute numbers
vax_numbers_long <- vax_numbers_long %>% 
  mutate(time = factor(time, levels = c("vaccinated_pre", "vaccinated_post")))
ggplot(vax_numbers_long, aes(x = state, y = vaccinations, fill = time)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.9)) +
  geom_text(aes(label = vaccinations), position = position_dodge(width=0.9), vjust = -0.5)+
  labs(title = "Vaccination Numbers by State", x = "State", y = "Number of Vaccinations") +
  theme_classic() +
  theme(legend.position = "bottom")

#Plots used for the modeling 

#As expected, there is a positive linear relationship between pre and post test score
ggplot(df_condensed, aes(x = mean_score_pre, y = mean_score_post, size = class_size)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + 
  scale_x_continuous(limits = c(2, 7)) +
  scale_y_continuous(limits = c(2, 7)) +
  labs(title = "Pre-test vs Post-test Mean Scores", x = "Pre-test Mean Score", y = "Post-test Mean Score") 


#Plot them by state. Suggests that we should be using random intercepts, as expected
ggplot(df_condensed, aes(x = mean_score_pre, y = mean_score_post, color = State, size=class_size)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + 
  scale_x_continuous(limits = c(2, 7)) +
  scale_y_continuous(limits = c(2, 7)) +
  labs(title = "Relationship Between Pre-test and Post-test Mean Scores by State",
       subtitle = "Color-coded by State",
       x = "Pre-test Mean Score",
       y = "Post-test Mean Score")
 

# Linear relationship seen 
ggplot(df_condensed, aes(x = change_score, y = change_vaccination_status, size = class_size)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + 
  labs(title = "Change in Mean Scores vs. Change in Vaccination Status",
       x = "Change in Mean Score",
       y = "Change in Vaccination Status (%)")

#Wierd negative value. will discuss w Praise
ggplot(df_condensed, aes(x = change_score, y = change_vaccination_status, color = State, size = class_size)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + 
  labs(title = "Change in Mean Scores vs. Change in Vaccination Status by State",
       subtitle = "Color-coded by State",
       x = "Change in Mean Score",
       y = "Change in Vaccination Status (%)")

#Remove the outlier
ggplot(df_condensed, aes(x = change_score, y = change_vaccination_status, color = State, size = class_size)) +
  geom_point(data = df_condensed %>% filter(change_score >= 0 & change_vaccination_status >= 0)) +
  geom_smooth(method = "lm", se = FALSE, data = df_condensed %>% filter(change_score >= 0 & change_vaccination_status >= 0)) +
  theme_minimal() + 
  labs(title = "Change in Mean Scores vs. Change in Vaccination Status by State",
       subtitle = "Color-coded by State",
       x = "Change in Mean Score",
       y = "Change in Vaccination Status (%)")

