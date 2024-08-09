#Analysis code
#Authors: Nicole Kim, BS and James Dickerson, MD MS 

library(dplyr)
library(stargazer)
library(car)
library(lmerTest)
library(performance)

setwd("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook")
df_total <- read.csv("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_/df_total_cleaned.csv")
df_pre <- read.csv("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_/df_pre_cleaned.csv")
df_post <- read.csv("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_/df_post_cleaned.csv")

#t tests 
fct <- filter(df_total, State == "FCT")
kaduna <- filter(df_total, State == "Kaduna")
rivers <- filter(df_total, State == "Rivers")
lagos <- filter(df_total, State == "Lagos")

t_test_data <- data.frame(state = c("fct", "kaduna", "rivers", "lagos"),
                          pre_score_mean = c(mean(fct$survey_score_pre, na.rm = TRUE),
                                             mean(kaduna$survey_score_pre, na.rm = TRUE),
                                             mean(rivers$survey_score_pre, na.rm = TRUE),
                                             mean(lagos$survey_score_pre, na.rm = TRUE)
                                             ),
                  post_score_mean = c(mean(fct$survey_score_post, na.rm = TRUE),
                                      mean(kaduna$survey_score_post, na.rm = TRUE),
                                      mean(rivers$survey_score_post, na.rm = TRUE),
                                      mean(lagos$survey_score_post, na.rm = TRUE)
                                      ),
                  vax_status_pre = c(sum(fct$vaccination_status_pre, na.rm = TRUE),
                                     sum(kaduna$vaccination_status_pre, na.rm = TRUE),
                                     sum(rivers$vaccination_status_pre, na.rm = TRUE),
                                     sum(lagos$vaccination_status_pre, na.rm = TRUE)
                                     ),
                  vax_status_post = c(sum(fct$vaccination_status_post, na.rm = TRUE),
                                      sum(kaduna$vaccination_status_post, na.rm = TRUE),
                                      sum(rivers$vaccination_status_post, na.rm = TRUE),
                                      sum(lagos$vaccination_status_post, na.rm = TRUE)
                                      )
                  )

t_test_fct <- df_pre %>% select(c(fct$survey_score_post, fct$survey_score_pre))
fct_post <- filter(df_post, State == "FCT")

score_change <- t.test(t_test_data[state=fct]$pre_score_mean, t_test_data[state=fct]$post_score_mean)



model1 <- lmer(survey_score_post ~ survey_score_pre + (1 | State), data = df_total)
summary(model1)

model2 <- lmer(survey_score_post ~ survey_score_pre + mother_education + (1 | State), data = df_total) 
summary(model2)

stargazer(model1)
