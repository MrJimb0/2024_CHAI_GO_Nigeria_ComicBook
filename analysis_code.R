#Analysis code
#Authors: Nicole Kim, BS and James Dickerson, MD MS 

library(readxl)
library(tidyverse)
library(stargazer)
library(car)
library(lmerTest)
library(performance)
library(ggplot2)

setwd("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook")
df_total <- read_xlsx("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_/df_total_cleaned.xlsx")
df_pre <- read_xlsx("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_/df_pre_cleaned.xlsx")
df_post <- read_xlsx("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_/df_post_cleaned.xlsx")

#t tests -- change in survey score pre and post intervention 
score_change <- t.test(df_post$survey_score, df_pre$survey_score)
score_wilcox <- wilcox.test(df_post$survey_score, df_pre$survey_score)

score_change_fct <- t.test(df_post[df_post$State == "FCT",]$survey_score, 
                       df_pre[df_pre$State == "FCT",]$survey_score)
score_change_kaduna <- t.test(df_post[df_post$State == "Kaduna",]$survey_score, 
                       t_test_pre[df_pre$State == "Kaduna",]$survey_score)
score_change_rivers <- t.test(df_post[df_post$State == "Rivers",]$survey_score, 
                              df_pre[df_pre$State == "Rivers",]$survey_score)
score_change_lagos <- t.test(df_post[df_post$State == "Lagos",]$survey_score, 
                              df_pre[df_pre$State == "Lagos",]$survey_score) 

graph_data1 <- df_total %>%
  select(State, Class, Age, survey_score_pre, survey_score_post)
graph_data1 <- graph_data1 %>%
  pivot_longer(cols = starts_with("survey_"),
               names_to = "score_type",
               values_to = "score_value") %>%
  mutate(score_type = recode(score_type, "survey_score_pre" = "pre", "survey_score_post" = "post"))

ggplot(graph_data1, aes(x=State, y=score_value, fill=interaction(State,score_type)))+
  geom_bar(stat="summary", position=position_dodge(width=0.9), na.rm=TRUE) +
  geom_errorbar(stat='summary', width=.2, position=position_dodge(width = 0.9))+
  theme_minimal()
  

score_change_jss1 <- t.test(df_post[df_post$Class == 1,]$survey_score, 
                       df_pre[df_pre$Class == 1,]$survey_score)
score_change_jss2 <- t.test(df_post[df_post$Class == 2,]$survey_score, 
                       t_test_pre[df_pre$Class == 2,]$survey_score)
  #no sig difference for jss2 vs. sig diff in Jss1/3
score_change_jss3 <- t.test(df_post[df_post$Class == 3,]$survey_score, 
                              df_pre[df_pre$Class == 3,]$survey_score)
#I don't think there are any SSS1 students in our dataset 
score_change_sss1 <- t.test(df_post[df_post$Class == 4,]$survey_score, 
                              df_pre[df_pre$Class == 4,]$survey_score)

score_change_10y <- t.test(df_post[df_post$Age == 10,]$survey_score, 
                            df_pre[df_pre$Age == 10,]$survey_score)
score_change_11y <- t.test(df_post[df_post$Age == 11,]$survey_score, 
                            t_test_pre[df_pre$Age == 11,]$survey_score)
  #no sig difference for 11yo only 
score_change_12y <- t.test(df_post[df_post$Age == 12,]$survey_score, 
                            df_pre[df_pre$Age == 12,]$survey_score)
score_change_13y <- t.test(df_post[df_post$Age == 13,]$survey_score, 
                           df_pre[df_pre$Age == 13,]$survey_score)
score_change_14y <- t.test(df_post[df_post$Age == 14,]$survey_score, 
                           t_test_pre[df_pre$Age == 14,]$survey_score)

#t-tests -- change in vaccination status pre and post intervention
vax_change <- t.test(df_post$vaccination_status, df_pre$vaccination_status)
vax_change_girls <- t.test(df_post[grepl("Girls", df_post$School),]$vaccination_status, df_pre[grepl("Girls", df_pre$School),]$vaccination_status)

vax_change <- chisq.test(df_total$vaccination_status_pre, df_total$vaccination_status_post)
  #need to fix 

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
pre_score_mean <- df_pre %>% group_by(State, School, Class) %>% summarise(mean_score_pre = mean(survey_score))
post_score_mean <- df_post %>% group_by(State,School,Class) %>% summarise(mean_score_post = mean(survey_score))
df_condensed <- merge(pre_score_mean, post_score_mean, by=c("State", "School", "Class"), all = TRUE)

model1 <- lmer(mean_score_pre ~ mean_score_post + (1 | State), data = df_condensed)
summary(model1)
#df_total$ME1_fit <- predict(model1)

model2 <- lmer(survey_score_post ~ survey_score_pre + mother_education + (1 | State), data = df_total) 
summary(model2)
df_total$ME2_fit <- predict(model2)


#anova 
model3 <- aov(mean_score_pre ~ mean_score_post + (1 | State), data = df_condensed)
summary(model3)
df_total$AOV_fit <- predict(model3)
