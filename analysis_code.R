#Analysis code
#Authors: Nicole Kim, BS and James Dickerson, MD MS 

library(tidyverse)
library(stargazer)
library(car)
library(lmerTest)
library(performance)

setwd("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook")
df_total <- read.csv("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_/df_total_cleaned.csv")
df_pre <- read.csv("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_/df_pre_cleaned.csv")
df_post <- read.csv("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_/df_post_cleaned.csv")

#t tests -- change in survey score pre and post intervention 
score_change <- t.test(df_post$survey_score, df_pre$survey_score)

score_change_fct <- t.test(df_post[df_post$State == "FCT",]$survey_score, 
                       df_pre[df_pre$State == "FCT",]$survey_score)
score_change_kaduna <- t.test(df_post[df_post$State == "Kaduna",]$survey_score, 
                       t_test_pre[df_pre$State == "Kaduna",]$survey_score)
score_change_rivers <- t.test(df_post[df_post$State == "Rivers",]$survey_score, 
                              df_pre[df_pre$State == "Rivers",]$survey_score)
score_change_lagos <- t.test(df_post[df_post$State == "Lagos",]$survey_score, 
                              df_pre[df_pre$State == "Lagos",]$survey_score)

score_change_jss1 <- t.test(df_post[df_post$Class == 1,]$survey_score, 
                       df_pre[df_pre$Class == 1,]$survey_score)
score_change_jss2 <- t.test(df_post[df_post$Class == 2,]$survey_score, 
                       t_test_pre[df_pre$Class == 2,]$survey_score)
score_change_jss3 <- t.test(df_post[df_post$Class == 3,]$survey_score, 
                              df_pre[df_pre$Class == 3,]$survey_score)
#I don't think there are any SSS1 students in our dataset 
score_change_sss1 <- t.test(df_post[df_post$Class == 4,]$survey_score, 
                              df_pre[df_pre$Class == 4,]$survey_score)

score_change_10y <- t.test(df_post[df_post$Age == 10,]$survey_score, 
                            df_pre[df_pre$Age == 10,]$survey_score)
score_change_11y <- t.test(df_post[df_post$Age == 11,]$survey_score, 
                            t_test_pre[df_pre$Age == 11,]$survey_score)
score_change_12y <- t.test(df_post[df_post$Age == 12,]$survey_score, 
                            df_pre[df_pre$Age == 12,]$survey_score)
score_change_13y <- t.test(df_post[df_post$Age == 13,]$survey_score, 
                           df_pre[df_pre$Age == 13,]$survey_score)
score_change_14y <- t.test(df_post[df_post$Age == 14,]$survey_score, 
                           t_test_pre[df_pre$Age == 14,]$survey_score)

#t-tests -- change in vaccination status pre and post intervention
vax_change <- t.test(df_post$vaccination_status, df_pre$vaccination_status)

vax_fct <- t.test(df_post[df_post$State == "FCT",]$vaccination_status, 
                           df_pre[df_pre$State == "FCT",]$vaccination_status)
vax_kaduna <- t.test(df_post[df_post$State == "Kaduna",]$vaccination_status, 
                              t_test_pre[df_pre$State == "Kaduna",]$vaccination_status)
vax_rivers <- t.test(df_post[df_post$State == "Rivers",]$vaccination_status, 
                              df_pre[df_pre$State == "Rivers",]$vaccination_status)
vax_lagos <- t.test(df_post[df_post$State == "Lagos",]$vaccination_status, 
                             df_pre[df_pre$State == "Lagos",]$vaccination_status)

vax_numbers <- data.frame(state = c("FCT", "Kaduna", "Rivers", "Lagos"), 
                          vaccinated_pre = c(sum(df_pre[df_pre$State == "FCT",]$vaccination_status, na.rm=TRUE),
                                             sum(df_pre[df_post$State == "Kaduna",]$vaccination_status, na.rm=TRUE),
                                             sum(df_pre[df_post$State == "Rivers",]$vaccination_status, na.rm=TRUE),
                                             sum(df_pre[df_post$State == "Lagos",]$vaccination_status, na.rm=TRUE)),
                          vaccinated_Post = c(sum(df_post[df_post$State == "FCT",]$vaccination_status, na.rm=TRUE),
                                              sum(df_post[df_post$State == "Kaduna",]$vaccination_status, na.rm=TRUE),
                                              sum(df_post[df_post$State == "Rivers",]$vaccination_status, na.rm=TRUE),
                                              sum(df_post[df_post$State == "Lagos",]$vaccination_status, na.rm=TRUE))
    )

#mixed effects 
model1 <- lmer(survey_score_post ~ survey_score_pre + (1 | State), data = df_total)
summary(model1)

model2 <- lmer(survey_score_post ~ survey_score_pre + mother_education + (1 | State), data = df_total) 
summary(model2)

stargazer(model1)
