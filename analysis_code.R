#Analysis code
#Authors: Nicole Kim, BS and James Dickerson, MD MS 
options(scipen=999)

library(Matrix)
library(readxl)
library(tidyverse)
library(lmerTest)
library(performance)
library(knitr)
library(broom)
library(ggplot2)
library(writexl)
library(table1)
library(sjPlot)

setwd("/Users/jamesdickerson/Library/CloudStorage/Box-Box/Dickerson Lab/Dickerson_Lab_Github/2024_CHAI_GO_Nigeria_ComicBook/Data_/Updated files from Nicole 8:17:24")
#setwd("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_")

df_pre <- read_xlsx("df_pre_cleaned.xlsx")
df_post <- read_xlsx("df_post_cleaned.xlsx")

#Summary statistics For Tables 
df_pre$parent_college <- ifelse(df_pre$father_education == "Bachelor's degree" | df_pre$father_education == "Postgraduate" | 
                                  df_pre$mother_education == "Bachelor's degree" | df_pre$mother_education == "Postgraduate", 
                                1, 0)
df_pre$Age <- as.numeric(df_pre$Age)

#summary stats -- table 1
summary_pre <- df_pre %>% group_by(State) %>%
  summarize(mean_age = mean(Age, na.rm=TRUE),
            sd_age = sd(Age, na.rm=TRUE),
            pct_christian = sum(Religion == "Christianity", na.rm = TRUE) / n() ,
            pct_muslim = sum(Religion == "Islam", na.rm = TRUE) / n())
summary_n <- data.frame(State = c("FCT", "Kaduna", "Rivers", "Lagos"),
             n_students_pre = c(nrow(df_pre[df_pre$State == "FCT",]), nrow(df_pre[df_pre$State == "Kaduna",]), 
                                 nrow(df_pre[df_pre$State == "Rivers",]), nrow(df_pre[df_pre$State == "Lagos",])),
             n_schools = c(2,2,4,2),
             pct_base_vaxed = c(mean(df_pre[df_pre$State == "FCT",]$vaccination_status, na.rm = TRUE), 
                              mean(df_pre[df_pre$State == "Kaduna",]$vaccination_status, na.rm = TRUE),
                              mean(df_pre[df_pre$State == "Rivers",]$vaccination_status, na.rm = TRUE), 
                              mean(df_pre[df_pre$State == "Lagos",]$vaccination_status, na.rm = TRUE)),
             pct_heardofHPV = c(mean(df_pre[df_pre$State == "FCT",]$heard_of_HPV, na.rm = TRUE), 
                              mean(df_pre[df_pre$State == "Kaduna",]$heard_of_HPV, na.rm = TRUE),
                              mean(df_pre[df_pre$State == "Rivers",]$heard_of_HPV, na.rm = TRUE), 
                              mean(df_pre[df_pre$State == "Lagos",]$heard_of_HPV, na.rm = TRUE)),
             pct_parent_college = c(mean(df_pre[df_pre$State == "FCT",]$parent_college, na.rm = TRUE), 
                              mean(df_pre[df_pre$State == "Kaduna",]$parent_college, na.rm = TRUE),
                              mean(df_pre[df_pre$State == "Rivers",]$parent_college, na.rm = TRUE), 
                              mean(df_pre[df_pre$State == "Lagos",]$parent_college, na.rm = TRUE))
             )  
summary_stats <- summary_pre %>%
  left_join(summary_n, by = c("State"))

summary_stats <- as.data.frame(rbind(summary_stats, 
                 data.frame(State = c("total"),
                            mean_age = c(mean(df_pre$Age)),
                            sd_age = c(sd(df_pre$Age)),
                            pct_christian = c(sum(df_pre$Religion == "Christianity", na.rm = TRUE) / nrow(df_pre)), 
                            pct_muslim = c(sum(df_pre$Religion == "Islam", na.rm = TRUE) / nrow(df_pre)), 
                            n_students_pre = c(nrow(df_pre)), 
                            pct_parent_college = c(mean(df_pre$parent_college, na.rm=TRUE)), 
                            n_schools = c(10), 
                            pct_base_vaxed = c(mean(df_pre$vaccination_status, na.rm = TRUE)),
                            pct_heardofHPV = c(mean(df_pre$heard_of_HPV, na.rm=TRUE))
                            )))
print(summary_stats)
#write_xlsx(summary_stats, path = "demographic_tablev1.xlsx")

#alt version 
# demographic_table <- table1(~Age + parent_college + Religion + survey_score + vaccination_status | State, data=df_pre, miss = 0)
# demographic_table.df <- as.data.frame(demosummary_table)
# write_xlsx(demographic_table.df, path = "demographic_table.xlsx")


#DATA SUMMARIES: 
#First, we look at baseline test scores by state. we expect a difference 
pre_post_test_scores_by_state <- rbind(
  df_pre %>% group_by(State) %>%
    summarise(
      Mean = mean(survey_score, na.rm = TRUE),
      SD = sd(survey_score, na.rm = TRUE),
      N = sum(!is.na(survey_score)),
      Time = "Pre-test"
    ),
  df_post %>% group_by(State) %>%
    summarise(
      Mean = mean(survey_score, na.rm = TRUE),
      SD = sd(survey_score, na.rm = TRUE),
      N = sum(!is.na(survey_score)),
      Time = "Post-test"
    )
)

kable(pre_post_test_scores_by_state, digits = 2)

#Then, we look at age, within each state
pre_post_test_scores_by_state_age <- rbind(
  df_pre %>% group_by(State, Age) %>%
    summarise(
      Mean = mean(survey_score, na.rm = TRUE),
      SD = sd(survey_score, na.rm = TRUE),
      N = sum(!is.na(survey_score)),
      Time = "Pre-test"
    ),
  df_post %>% group_by(State, Age) %>%
    summarise(
      Mean = mean(survey_score, na.rm = TRUE),
      SD = sd(survey_score, na.rm = TRUE),
      N = sum(!is.na(survey_score)),
      Time = "Post-test"
    )
)

kable(pre_post_test_scores_by_state_age, digits = 2)

#Then, we look at school, within each state
pre_post_test_scores_by_state_school <- rbind(
  df_pre %>% group_by(State, School) %>%
    summarise(
      Mean = mean(survey_score, na.rm = TRUE),
      SD = sd(survey_score, na.rm = TRUE),
      N = sum(!is.na(survey_score)),
      Time = "Pre-test"
    ),
  df_post %>% group_by(State, School) %>%
    summarise(
      Mean = mean(survey_score, na.rm = TRUE),
      SD = sd(survey_score, na.rm = TRUE),
      N = sum(!is.na(survey_score)),
      Time = "Post-test"
    )
)

kable(pre_post_test_scores_by_state_school, digits = 2)

#look at each class 
  #There is a lot of unevenness in the representation of the classes For example, one class in Abuja has 8 in the pre-test and 20 in the post-test
  #So we have a sampling error within the schools that we need to look at and discuss as a major limitation
pre_post_test_scores_by_state_school_class <- rbind(
  df_pre %>% group_by(State, School, Class) %>%
    summarise(
      Mean = mean(survey_score, na.rm = TRUE),
      SD = sd(survey_score, na.rm = TRUE),
      N = sum(!is.na(survey_score)),
      Time = "Pre_test"
    ),
  df_post %>% group_by(State, School, Class) %>%
    summarise(
      Mean = mean(survey_score, na.rm = TRUE),
      SD = sd(survey_score, na.rm = TRUE),
      N = sum(!is.na(survey_score)),
      Time = "Post_test"
    )
) %>% pivot_wider(names_from = "Time", values_from = c(Mean, SD, N))

pre_post_test_scores_by_state_school_class$N_Post_test[is.na(pre_post_test_scores_by_state_school_class$N_Post_test)] = 0

kable(pre_post_test_scores_by_state_school_class, digits = 2)

pre_post_test_scores_by_state_school_class$`N_Post_test`

#relationship between parental education and pre-test score both for mom and dad 
#NA answers for parent education were not counted, note that none was a seperate option
father_edu_vs_pre_test_score <- df_pre %>% 
  filter(!is.na(father_education)) %>%
  group_by(State, father_education) %>%
  summarize(mean = mean(survey_score, na.rm=TRUE), sd = sd(survey_score, na.rm=TRUE))
father_edu_vs_pre_test_score$parent <- "father"
father_edu_vs_pre_test_score <- father_edu_vs_pre_test_score %>% rename(edu_level = father_education)
     
mother_edu_vs_pre_test_score <- df_pre %>%
  filter(!is.na(mother_education)) %>%
  group_by(State, mother_education) %>%
  summarize(mean = mean(survey_score, na.rm=TRUE), sd = sd(survey_score, na.rm=TRUE))
mother_edu_vs_pre_test_score$parent <- "mother"
mother_edu_vs_pre_test_score <- mother_edu_vs_pre_test_score %>% rename(edu_level = mother_education)

parent_edu_vs_pre_test_score <- rbind(mother_edu_vs_pre_test_score, father_edu_vs_pre_test_score)
df_edu <- parent_edu_vs_pre_test_score
kable(df_edu, digits = 2)

#T-TESTS:
#All results for knowledge gain are significant with improvements in scores 
#Overall T test showing improvement in test scores for everyone
overall_t <- t.test(df_post$survey_score, df_pre$survey_score)
kable(tidy(overall_t), digits = 2, caption = "Overall T-test Results")
# by state
t_tests <- list(
  FCT = t.test(df_post[df_post$State == "FCT",]$survey_score, df_pre[df_pre$State == "FCT",]$survey_score),
  Kaduna = t.test(df_post[df_post$State == "Kaduna",]$survey_score, df_pre[df_pre$State == "Kaduna",]$survey_score),
  Rivers = t.test(df_post[df_post$State == "Rivers",]$survey_score, df_pre[df_pre$State == "Rivers",]$survey_score),
  Lagos = t.test(df_post[df_post$State == "Lagos",]$survey_score, df_pre[df_pre$State == "Lagos",]$survey_score)
)

# Combine the results into a single data frame
results_df <- do.call(rbind, lapply(t_tests, tidy))
# Add a column for the name
results_df$Stratification_Factor <- names(t_tests)

#Output tables for score improvement t-test
kable(results_df, digits = 2)


#score improvement as pct -- Table 2
df_pre$survey_pct <- df_pre$survey_score/7
df_post$survey_pct <- df_post$survey_score/7

FCT_pct_htest = t.test(df_post[df_post$State == "FCT",]$survey_pct, df_pre[df_pre$State == "FCT",]$survey_pct)
Kaduna_pct_htest = t.test(df_post[df_post$State == "Kaduna",]$survey_pct, df_pre[df_pre$State == "Kaduna",]$survey_pct)
Rivers_pct_htest = t.test(df_post[df_post$State == "Rivers",]$survey_pct, df_pre[df_pre$State == "Rivers",]$survey_pct)
Lagos_pct_htest = t.test(df_post[df_post$State == "Lagos",]$survey_pct, df_pre[df_pre$State == "Lagos",]$survey_pct)
overall_pct_htest = t.test(df_post$survey_pct, df_pre$survey_pct)

summary_df3 <- data.frame(state = c("FCT", "Kaduna", "Lagos", "Rivers", "Overall"),
                          mean_pre = c(mean(df_pre[df_pre$State == "FCT",]$survey_pct, na.rm=TRUE),
                                       mean(df_pre[df_pre$State == "Kaduna",]$survey_pct, na.rm=TRUE),
                                       mean(df_pre[df_pre$State == "Lagos",]$survey_pct, na.rm=TRUE),
                                       mean(df_pre[df_pre$State == "Rivers",]$survey_pct, na.rm=TRUE),
                                       mean(df_pre$survey_pct, na.rm=TRUE)),
                          sd_pre = c(sd(df_pre[df_pre$State == "FCT",]$survey_pct, na.rm=TRUE),
                                     sd(df_pre[df_pre$State == "Kaduna",]$survey_pct, na.rm=TRUE),
                                     sd(df_pre[df_pre$State == "Lagos",]$survey_pct, na.rm=TRUE),
                                     sd(df_pre[df_pre$State == "Rivers",]$survey_pct, na.rm=TRUE),
                                     sd(df_pre$survey_pct, na.rm=TRUE)),
                          mean_post = c(mean(df_post[df_post$State == "FCT",]$survey_pct, na.rm=TRUE),
                                        mean(df_post[df_post$State == "Kaduna",]$survey_pct, na.rm=TRUE),
                                        mean(df_post[df_post$State == "Lagos",]$survey_pct, na.rm=TRUE),
                                        mean(df_post[df_post$State == "Rivers",]$survey_pct, na.rm=TRUE),
                                        mean(df_post$survey_pct, na.rm=TRUE)),
                          sd_post = c(sd(df_post[df_post$State == "FCT",]$survey_pct, na.rm=TRUE),
                                      sd(df_post[df_post$State == "Kaduna",]$survey_pct, na.rm=TRUE),
                                      sd(df_post[df_post$State == "Lagos",]$survey_pct, na.rm=TRUE),
                                      sd(df_post[df_post$State == "Rivers",]$survey_pct, na.rm=TRUE),
                                      sd(df_post$survey_pct, na.rm=TRUE)),
                          p_value = c(FCT_pct_htest$p.value, Kaduna_pct_htest$p.value, Lagos_pct_htest$p.value, Rivers_pct_htest$p.value, overall_pct_htest$p.value)
)
kable(summary_df3, digits = 2)
write_xlsx(summary_df3, path = "summary_figure3.xlsx")

#t-tests for perception of vaccine
FCT_percep.htest = t.test(df_post[df_post$State == "FCT",]$perception, df_pre[df_pre$State == "FCT",]$perception)
Kaduna_percep.htest = t.test(df_post[df_post$State == "Kaduna",]$perception, df_pre[df_pre$State == "Kaduna",]$perception)
Rivers_percep.htest = t.test(df_post[df_post$State == "Rivers",]$perception, df_pre[df_pre$State == "Rivers",]$perception)
Lagos_percep.htest = t.test(df_post[df_post$State == "Lagos",]$perception, df_pre[df_pre$State == "Lagos",]$perception)
overall_percep.htest = t.test(df_post$perception, df_pre$perception)

#Perception of vaccine raw percentage
summary(df_pre[df_pre$State == "FCT",]$perception)
summary(df_post[df_post$State == "FCT",]$perception)
summary(df_pre[df_pre$State == "Kaduna",]$perception)
summary(df_post[df_post$State == "Kaduna",]$perception)
summary(df_pre[df_pre$State == "Rivers",]$perception)
summary(df_post[df_post$State == "Rivers",]$perception)
summary(df_pre[df_pre$State == "Lagos",]$perception)
summary(df_post[df_post$State == "Lagos",]$perception)
summary(df_pre$perception)
summary(df_post$perception)

perception_df <- data.frame(
  State = c("Overall", "FCT", "Kaduna", "Rivers", "Lagos"),
  Pre = c(
    0.6176,
    0.8315,
    0.4845,
    0.6226,
    0.5474
  ),
  Post = c(
    0.8104,
    0.7935,
    0.8081,
    0.94,
    0.6915
  )
)

perception_df
#Then T test for overall vaccination status
overall_vax.ttest = t.test(df_post$vaccination_status, df_pre$vaccination_status)
overall_vax.ttest
#Then T test for overall perception of vaccine
overall_percep.ttest = t.test(df_post$perception, df_pre$perception)
overall_percep.ttest

#MIXED EFFECTS MODELS:
#Set up of a condensed df. Decided to condense at the level of the class as that is the smallest unit we have 
pre_score_mean <- df_pre %>% group_by(State, School, Class) %>% summarise(mean_score_pre = mean(survey_score, na.rm=TRUE))
post_score_mean <- df_post %>% group_by(State,School,Class) %>% summarise(mean_score_post = mean(survey_score, na.rm=TRUE))
df_condensed <- merge(pre_score_mean, post_score_mean, by=c("State", "School", "Class"), all = TRUE)

#We will also look at the change in vaccination status. we know that Rivers/Kaduna had no vaccine prior to the intervention
#So any NAs can be changed to 0 for pre
df_pre_vaccination <- df_pre %>% group_by(State, School, Class) %>% 
  summarise(pre_vaccination_status = mean(vaccination_status, na.rm=T),
            pre_perception = mean(perception, na.rm=T))
df_post_vaccination <- df_post %>% group_by(State, School, Class) %>% 
  summarise(post_vaccination_status = mean(vaccination_status, na.rm=T),
            post_perception = mean(perception, na.rm=T))

df_condensed <- merge(df_condensed, df_pre_vaccination, by=c("State", "School", "Class"), all = TRUE)
df_condensed <- merge(df_condensed, df_post_vaccination, by=c("State", "School", "Class"), all = TRUE)

df_condensed <- df_condensed %>%
  mutate(pre_vaccination_status = ifelse(State %in% c("Rivers", "Kaduna") & is.na(pre_vaccination_status), 0, pre_vaccination_status))

df_condensed <- df_condensed %>%
  mutate(pre_vaccination_status = pre_vaccination_status * 100,
         post_vaccination_status = post_vaccination_status * 100,
         change_vaccination_status = post_vaccination_status - pre_vaccination_status,
         change_perception = post_perception - pre_perception,
         change_score = mean_score_post - mean_score_pre)

#Add in the number of surveyed kids per class. 
    #Because the numbers were different each time, we took an average of the number pre and post 
df_condensed <- df_condensed %>%
  left_join(pre_post_test_scores_by_state_school_class, by = c("State", "School", "Class")) 

#Rename these to make the rowMeans function easier to use 
df_condensed <- df_condensed %>% rename(n_pretest = N_Pre_test, n_posttest = N_Post_test)
df_condensed$class_size <- rowMeans(df_condensed[, c("n_pretest", "n_posttest")], na.rm = TRUE)

#add parent education to condensed data 
#only did parent edu for kids who took survey at endline
df_post$mother_college <- ifelse(df_post$mother_education == "Bachelor's degree" | df_post$mother_education == "Postgraduate", 
                                1, 0)
df_post$father_college <- ifelse(df_post$father_education == "Bachelor's degree" | df_post$father_education == "Postgraduate", 
                                1, 0)

df_post$parent_college <- ifelse(df_post$father_education == "Bachelor's degree" | df_post$father_education == "Postgraduate" | 
                                   df_post$mother_education == "Bachelor's degree" | df_post$mother_education == "Postgraduate", 
                                 1, 0)

df_parentedu <- df_post %>% group_by(State, School, Class) %>% summarise(parent_college = mean(parent_college, na.rm=TRUE))
df_condensed <- df_condensed %>%
  left_join(df_parentedu, by = c("State", "School", "Class")) 


#Write it for graphing
write_xlsx(df_condensed, "df_condensed.xlsx")

#This model is pre test predicts post test. which is pretty useless other than as a data check
model1 <- glm(mean_score_post ~ mean_score_pre, data = df_condensed, weights = class_size)
model2 <- lmer(mean_score_post ~ mean_score_pre + (1 | State), data = df_condensed, weights = class_size)
summary(model1)
summary(model2)

plot_model(model2, type = "pred", terms = "mean_score_pre")
ggplot(df_condensed, aes(x = mean_score_pre, y = mean_score_post, size = class_size)) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Mean Score Pre", 
       y = "Mean Score Post") + 
  theme_classic()

#Does Pre-test score predict score change ie learning?
model3 <- lmer(change_score ~ mean_score_pre + (1 | State), data = df_condensed, weights = class_size)
summary(model3)

ggplot(df_condensed, aes(x = mean_score_pre, y = change_score, size = class_size, color = State)) +
  geom_point(aes(color = State), alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Mean Score Pre", 
       y = "Change Score") + 
  theme_classic()
#Results show learning w downward slopes for all. if score of 3 then expect a change of 1.5. If score of 5 then expect a change of 0.5
#Of course there is a cieling but effect is fairly linear 

#Now we ask if change in the test score, vaccine perception, or post-perception is predictive of change in vaccination status and post-vaccine status
model4 <- lmer(change_vaccination_status ~ change_score + (1 | State), data = df_condensed, weights = class_size)
summary(model4)

#No relationships really other than kaduna. However, we now there are bad data, for example the kids who reported they were already vaccinated
ggplot(df_condensed, aes(x = change_score, y = change_vaccination_status, size = class_size, color = State)) +
  geom_point(aes(color = State), alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Change in Test Score", 
       y = "Change in Vaccination Status") + 
  theme_classic() 

#Re-run model 4 with the -22% removed
model4vers2 <- lmer(change_vaccination_status ~ change_score + (1 | State), 
               data = df_condensed %>% filter(change_score >= 0 & change_vaccination_status >= 0), 
               weights = class_size)
summary(model4vers2)
#Still no relationship. Not suprising as we know there are issues with the data and we are looking at education, not effect


#models with vaccine perception 
model6 <- lmer(change_vaccination_status ~ change_perception + (1 | State), data = df_condensed, weights = class_size)
summary(model6)

ggplot(df_condensed, aes(x = change_perception, y = change_vaccination_status, size = class_size, color= State)) +
  geom_point(aes(color = State), alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Change in Perception", 
       y = "Change in Vaccination Status") + 
  theme_classic()

model7 <- lmer(change_vaccination_status ~ post_perception + (1 | State), data = df_condensed, weights = class_size)
summary(model7)

ggplot(df_condensed, aes(x = post_perception, y = change_vaccination_status, size = class_size, color= State)) +
  geom_point(aes(color = State), alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Post Perception", 
       y = "Change in Vaccination Status") + 
  theme_classic()

model8 <- lmer(change_perception ~ change_score + (1 | State), data = df_condensed, weights = class_size)
summary(model8)

ggplot(df_condensed, aes(x = change_score, y = change_perception, size = class_size, color= State)) +
  geom_point(aes(color = State), alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Change in Test Score", 
       y = "Change in Perception") + 
  theme_classic()




