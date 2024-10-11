#Analysis code
#Authors: Nicole Kim, BS and James Dickerson, MD MS 
options(scipen=999)

library(Matrix)
library(readxl)
library(tidyverse)
library(stargazer)
library(car)
library(lmerTest)
library(performance)
library(knitr)
library(broom)
library(ggplot2)
library(writexl)
library(table1)

#setwd("/Users/jamesdickerson/Library/CloudStorage/Box-Box/Dickerson Lab/Dickerson_Lab_Github/2024_CHAI_GO_Nigeria_ComicBook/Data_/Updated files from Nicole 8:17:24")
setwd("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook")

df_total <- read_xlsx("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_/df_total_cleaned.xlsx")
df_pre <- read_xlsx("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_/df_pre_cleaned.xlsx")
df_post <- read_xlsx("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_/df_post_cleaned.xlsx")

#Summary statistics For Tables 
df_pre$parent_college <- ifelse(df_pre$father_education == "Bachelor's degree" | df_pre$father_education == "Postgraduate" | 
                                  df_pre$mother_education == "Bachelor's degree" | df_pre$mother_education == "Postgraduate", 
                                1, 0)
df_pre$Age <- as.numeric(df_pre$Age)

#summary stats table v1
summary_pre <- df_pre %>% group_by(State) %>%
  summarize(mean_age = mean(Age, na.rm=TRUE),
            pct_christian = sum(Religion == "Christianity", na.rm = TRUE) / n() ,
            pct_muslim = sum(Religion == "Islam", na.rm = TRUE) / n())
summary_n <- data.frame(State = c("FCT", "Kaduna", "Rivers", "Lagos"),
             n_students_pre = c(nrow(df_pre[df_pre$State == "FCT",]), nrow(df_pre[df_pre$State == "Kaduna",]), 
                                 nrow(df_pre[df_pre$State == "Rivers",]), nrow(df_pre[df_pre$State == "Lagos",])), 
             n_students_post = c(nrow(df_post[df_post$State == "FCT",]), nrow(df_post[df_post$State == "Kaduna",]), 
                                 nrow(df_post[df_post$State == "Rivers",]), nrow(df_post[df_post$State == "Lagos",])), 
             n_schools = c(2,2,4,2),
             pct_base_vaxed = c(mean(df_pre[df_pre$State == "FCT",]$vaccination_status, na.rm = TRUE), 
                              mean(df_pre[df_pre$State == "Kaduna",]$vaccination_status, na.rm = TRUE),
                              mean(df_pre[df_pre$State == "Rivers",]$vaccination_status, na.rm = TRUE), 
                              mean(df_pre[df_pre$State == "Lagos",]$vaccination_status, na.rm = TRUE)),
             pct_parent_college = c(mean(df_pre[df_pre$State == "FCT",]$parent_college, na.rm = TRUE), 
                              mean(df_pre[df_pre$State == "Kaduna",]$parent_college, na.rm = TRUE),
                              mean(df_pre[df_pre$State == "Rivers",]$parent_college, na.rm = TRUE), 
                              mean(df_pre[df_pre$State == "Lagos",]$parent_college, na.rm = TRUE))
             )  
summary_stats <- summary_pre %>%
  left_join(summary_n, by = c("State"))

n_pre <- 388
summary_stats <- as.data.frame(rbind(summary_stats, 
                 data.frame(State = c("total"),
                            mean_age = c(mean(df_pre$Age)), 
                            pct_christian = c(sum(df_pre$Religion == "Christianity", na.rm = TRUE) / n_pre), 
                            pct_muslim = c(sum(df_pre$Religion == "Islam", na.rm = TRUE) / n_pre), 
                            n_students_pre = c(nrow(df_pre)), n_students_post = c(nrow(df_post)),
                            pct_parent_college = c(mean(df_pre$parent_college, na.rm=TRUE)), 
                            n_schools = c(10), 
                            pct_base_vaxed = c(mean(df_pre$vaccination_status, na.rm = TRUE))
                            )))
print(summary_stats)
write_xlsx(summary_stats, path = "demographic_tablev1.xlsx")


#alt version 
demographic_table <- table1(~Age + parent_college + Religion + survey_score + vaccination_status | State, data=df_pre, miss = 0)
demographic_table.df <- as.data.frame(demosummary_table)
write_xlsx(demographic_table.df, path = "demographic_table.xlsx")

#Overall T test showing improvement in test scores for everyone
overall_t <- (t.test(df_post$survey_score, df_pre$survey_score))
kable(tidy(overall_t), digits = 2, caption = "Overall T-test Results")

#First, we look at baseline test scores by state. we expect a difference 
pre_post_test_scores_by_state <- rbind(
  df_pre %>%
    group_by(State) %>%
    summarise(
      Mean = mean(survey_score, na.rm = TRUE),
      SD = sd(survey_score, na.rm = TRUE),
      N = sum(!is.na(survey_score)),
      Time = "Pre-test"
    ),
  df_post %>%
    group_by(State) %>%
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
  df_pre %>%
    group_by(State, Age) %>%
    summarise(
      Mean = mean(survey_score, na.rm = TRUE),
      SD = sd(survey_score, na.rm = TRUE),
      N = sum(!is.na(survey_score)),
      Time = "Pre-test"
    ),
  df_post %>%
    group_by(State, Age) %>%
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
  df_pre %>%
    group_by(State, School) %>%
    summarise(
      Mean = mean(survey_score, na.rm = TRUE),
      SD = sd(survey_score, na.rm = TRUE),
      N = sum(!is.na(survey_score)),
      Time = "Pre-test"
    ),
  df_post %>%
    group_by(State, School) %>%
    summarise(
      Mean = mean(survey_score, na.rm = TRUE),
      SD = sd(survey_score, na.rm = TRUE),
      N = sum(!is.na(survey_score)),
      Time = "Post-test"
    )
)

kable(pre_post_test_scores_by_state_school, digits = 2)

#We look at each class 
#There is a lot of unevenness in the representation of the classes For example, one class in Abuja has 8 in the pre-test and 20 in the post-test
#So we have a sampling error within the schools that we need to look at and discuss as a major limitation
pre_post_test_scores_by_state_school_class <- rbind(
  df_pre %>%
    group_by(State, School, Class) %>%
    summarise(
      Mean = mean(survey_score, na.rm = TRUE),
      SD = sd(survey_score, na.rm = TRUE),
      N = sum(!is.na(survey_score)),
      Time = "Pre-test"
    ),
  df_post %>%
    group_by(State, School, Class) %>%
    summarise(
      Mean = mean(survey_score, na.rm = TRUE),
      SD = sd(survey_score, na.rm = TRUE),
      N = sum(!is.na(survey_score)),
      Time = "Post-test"
    )
) %>%
  pivot_wider(names_from = "Time", values_from = c(Mean, SD, N))

kable(pre_post_test_scores_by_state_school_class, digits = 2)

pre_post_test_scores_by_state_school_class$`N_Post-test`

#Now, we want to look at the relationship between parental education and pre-test score both for mom and dad 
  #NA answers for parent education were not counted  
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
            
#Next, we do a bunch of t tests. All are significant with improvements in scores 
FCT.htest = t.test(df_post[df_post$State == "FCT",]$survey_score, df_pre[df_pre$State == "FCT",]$survey_score)
Kaduna.htest = t.test(df_post[df_post$State == "Kaduna",]$survey_score, df_pre[df_pre$State == "Kaduna",]$survey_score)
Rivers.htest = t.test(df_post[df_post$State == "Rivers",]$survey_score, df_pre[df_pre$State == "Rivers",]$survey_score)
Lagos.htest = t.test(df_post[df_post$State == "Lagos",]$survey_score, df_pre[df_pre$State == "Lagos",]$survey_score)
overall.htest = t.test(df_post$survey_score, df_pre$survey_score)


t_tests <- list(
  FCT = tidy(FCT.htest),
  Kaduna = tidy(Kaduna.htest),
  Rivers = tidy(Rivers.htest),
  Lagos = tidy(Lagos.htest),
  JSS1 = tidy(t.test(df_post[df_post$Class == 1,]$survey_score, df_pre[df_pre$Class == 1,]$survey_score)),
  JSS2 = tidy(t.test(df_post[df_post$Class == 2,]$survey_score, df_pre[df_pre$Class == 2,]$survey_score)),
  JSS3 = tidy(t.test(df_post[df_post$Class == 3,]$survey_score, df_pre[df_pre$Class == 3,]$survey_score)),
 # SSS1 = tidy(t.test(df_post[df_post$Class == 4,]$survey_score, df_pre[df_pre$Class == 4,]$survey_score)),
  #Sparse data for SSSI so did not compare it by itself 
  Age_10 = tidy(t.test(df_post[df_post$Age == 10,]$survey_score, df_pre[df_pre$Age == 10,]$survey_score)),
  Age_11 = tidy(t.test(df_post[df_post$Age == 11,]$survey_score, df_pre[df_pre$Age == 11,]$survey_score)),
  Age_12 = tidy(t.test(df_post[df_post$Age == 12,]$survey_score, df_pre[df_pre$Age == 12,]$survey_score)),
  Age_13 = tidy(t.test(df_post[df_post$Age == 13,]$survey_score, df_pre[df_pre$Age == 13,]$survey_score)),
  Age_14 = tidy(t.test(df_post[df_post$Age == 14,]$survey_score, df_pre[df_pre$Age == 14,]$survey_score))
)

# Combine the results into a single data frame
results_df <- do.call(rbind, t_tests)
# Add a column for the name
results_df$Stratification_Factor <- names(t_tests)

#Output table for score improvement t-test
kable(results_df, digits = 2)

library(table1)
summary_table1 <- table1(~mean_score_pre + mean_score_post + n_posttest | State, data=df_condensed, miss = 0)
summary_df1 <- as.data.frame(summary_table1)
write_xlsx(summary_df1, path = "summary_table1.xlsx")

#USE THIS OUTPUT TABLE -- table for t-test results for score improvement 
summary_df2 <- data.frame(state = c("FCT", "Kaduna", "Lagos", "Rivers", "Overall"),
                          mean_pre = c(mean(df_pre[df_pre$State == "FCT",]$survey_score, na.rm=TRUE),
                                       mean(df_pre[df_pre$State == "Kaduna",]$survey_score, na.rm=TRUE),
                                       mean(df_pre[df_pre$State == "Lagos",]$survey_score, na.rm=TRUE),
                                       mean(df_pre[df_pre$State == "Rivers",]$survey_score, na.rm=TRUE),
                                       mean(df_pre$survey_score, na.rm=TRUE)),
                          sd_pre = c(sd(df_pre[df_pre$State == "FCT",]$survey_score, na.rm=TRUE),
                                     sd(df_pre[df_pre$State == "Kaduna",]$survey_score, na.rm=TRUE),
                                     sd(df_pre[df_pre$State == "Lagos",]$survey_score, na.rm=TRUE),
                                     sd(df_pre[df_pre$State == "Rivers",]$survey_score, na.rm=TRUE),
                                     sd(df_pre$survey_score, na.rm=TRUE)),
                          mean_post = c(mean(df_post[df_post$State == "FCT",]$survey_score, na.rm=TRUE),
                                        mean(df_post[df_post$State == "Kaduna",]$survey_score, na.rm=TRUE),
                                        mean(df_post[df_post$State == "Lagos",]$survey_score, na.rm=TRUE),
                                        mean(df_post[df_post$State == "Rivers",]$survey_score, na.rm=TRUE),
                                        mean(df_post$survey_score, na.rm=TRUE)),
                          sd_post = c(sd(df_post[df_post$State == "FCT",]$survey_score, na.rm=TRUE),
                                      sd(df_post[df_post$State == "Kaduna",]$survey_score, na.rm=TRUE),
                                      sd(df_post[df_post$State == "Lagos",]$survey_score, na.rm=TRUE),
                                      sd(df_post[df_post$State == "Rivers",]$survey_score, na.rm=TRUE),
                                      sd(df_post$survey_score, na.rm=TRUE)),
                          p_value = c(FCT.htest$p.value, Kaduna.htest$p.value, Lagos.htest$p.value, Rivers.htest$p.value, overall.htest$p.value)
                          )
write_xlsx(summary_df2, path = "summary_figure2.xlsx")

#t-tests for perception of vaccine
FCT_percep.htest = t.test(df_post[df_post$State == "FCT",]$perception, df_pre[df_pre$State == "FCT",]$perception)
Kaduna_percep.htest = t.test(df_post[df_post$State == "Kaduna",]$perception, df_pre[df_pre$State == "Kaduna",]$perception)
Rivers_percep.htest = t.test(df_post[df_post$State == "Rivers",]$perception, df_pre[df_pre$State == "Rivers",]$perception)
Lagos_percep.htest = t.test(df_post[df_post$State == "Lagos",]$perception, df_pre[df_pre$State == "Lagos",]$perception)
overall_percep.htest = t.test(df_post$perception, df_pre$perception)


#Next, we look at ∆ in vaccination status, stratifying by state and also looking at girl only schools (since we didn't record sex of the children)
vax_tests <- list(
  FCT = tidy(prop.test(x = sum(df_post[df_post$State == "FCT", ]$vaccination_status, na.rm = TRUE), 
                       n = sum(!is.na(df_post[df_post$State == "FCT", ]$vaccination_status)), 
                       p = sum(df_pre[df_pre$State == "FCT", ]$vaccination_status, na.rm = TRUE) / sum(!is.na(df_pre[df_pre$State == "FCT", ]$vaccination_status)), 
                       alternative = "two.sided")),
  Kaduna = tidy(prop.test(x = sum(df_post[df_post$State == "Kaduna", ]$vaccination_status, na.rm = TRUE), 
                          n = sum(!is.na(df_post[df_post$State == "Kaduna", ]$vaccination_status)), 
                          p = sum(df_pre[df_pre$State == "Kaduna", ]$vaccination_status, na.rm = TRUE) / sum(!is.na(df_pre[df_pre$State == "Kaduna", ]$vaccination_status)), 
                          alternative = "two.sided")),
  Rivers = tidy(prop.test(x = sum(df_post[df_post$State == "Rivers", ]$vaccination_status, na.rm = TRUE), 
                          n = sum(!is.na(df_post[df_post$State == "Rivers", ]$vaccination_status)), 
                          p = sum(df_pre[df_pre$State == "Rivers", ]$vaccination_status, na.rm = TRUE) / sum(!is.na(df_pre[df_pre$State == "Rivers", ]$vaccination_status)), 
                          alternative = "two.sided")),
  Lagos = tidy(prop.test(x = sum(df_post[df_post$State == "Lagos", ]$vaccination_status, na.rm = TRUE), 
                         n = sum(!is.na(df_post[df_post$State == "Lagos", ]$vaccination_status)), 
                         p = sum(df_pre[df_pre$State == "Lagos", ]$vaccination_status, na.rm = TRUE) / sum(!is.na(df_pre[df_pre$State == "Lagos", ]$vaccination_status)), 
                         alternative = "two.sided")),
  Girls = tidy(prop.test(x = sum(df_post[grepl("Girls", df_post$School), ]$vaccination_status, na.rm = TRUE), 
                         n = sum(!is.na(df_post[grepl("Girls", df_post$School), ]$vaccination_status)), 
                         p = sum(df_pre[grepl("Girls", df_pre$School), ]$vaccination_status, na.rm = TRUE) / sum(!is.na(df_pre[grepl("Girls", df_pre$School), ]$vaccination_status)), 
                         alternative = "two.sided"))
)

# Combine the results into a single data frame
vax_results_df <- do.call(rbind, vax_tests)

# Add a column for the group
vax_results_df$Group <- names(vax_tests)

# Vaccination numbers by state and girls
vax_numbers <- data.frame(
  state = c("FCT", "Kaduna", "Rivers", "Lagos", "Girls"),
  vaccinated_pre = sapply(c("FCT", "Kaduna", "Rivers", "Lagos", "Girls"), function(x) {
    if (x == "Girls") {
      sum(df_pre[grepl("Girls", df_pre$School),]$vaccination_status, na.rm=TRUE)
    } else {
      sum(df_pre[df_pre$State == x,]$vaccination_status, na.rm=TRUE)
    }
  }),
  vaccinated_post = sapply(c("FCT", "Kaduna", "Rivers", "Lagos", "Girls"), function(x) {
    if (x == "Girls") {
      sum(df_post[grepl("Girls", df_post$School),]$vaccination_status, na.rm=TRUE)
    } else {
      sum(df_post[df_post$State == x,]$vaccination_status, na.rm=TRUE)
    }
  })
)
# Use kable to summarize the results
# these results should NOT be seen as causitive 
#Improvement in girls being vaccinated and in all states BUT many confounders 
kable(vax_numbers, digits = 2, caption = "Vaccination Numbers by State")
kable(vax_results_df, digits = 2, caption = "Proportion Test Results")

#Set up ggplot by creating long form file
vax_numbers_long <- vax_numbers %>%
  pivot_longer(cols = c(vaccinated_pre, vaccinated_post), names_to = "time", values_to = "vaccinations")
write_xlsx(vax_numbers_long, "vax_numbers_long.xlsx")


#Mixed effects Models

#Set up of a condensed df. Decided to condense at the level of the class as that is the smallest unit we have 

#Because of the inconsistently in the classes being reported, will start with class but may change to school as the unit of collapse (see line 96)
#Lets do means of the scores for each class and add that to our condensed df 
pre_score_mean <- df_pre %>% group_by(State, School, Class) %>% summarise(mean_score_pre = mean(survey_score))
post_score_mean <- df_post %>% group_by(State,School,Class) %>% summarise(mean_score_post = mean(survey_score))
df_condensed <- merge(pre_score_mean, post_score_mean, by=c("State", "School", "Class"), all = TRUE)

#We will also look at the change in vaccination status. we know that Rivers/Kaduna had no vaccine prior to the intervention
#So any NAs can be changed to 0 for pre
df_pre_vaccination <- df_pre %>% group_by(State, School, Class) %>% 
  summarise(pre_vaccination_status = mean(vaccination_status),
            pre_perception = mean(perception))
df_post_vaccination <- df_post %>% group_by(State, School, Class) %>% 
  summarise(post_vaccination_status = mean(vaccination_status),
            post_perception = mean(perception))

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
df_condensed$n_pretest <- df_condensed$`N_Pre-test`
df_condensed$n_posttest <- df_condensed$`N_Post-test`
df_condensed$class_size <- rowMeans(df_condensed[, c("n_pretest", "n_posttest")], na.rm = TRUE)

#add parent education to condensed data 
#only did parent edu for kids who took survey at endline. could change to baseline or include both 
df_post$mother_college <- ifelse(df_post$mother_education == "Bachelor's degree" | df_post$mother_education == "Postgraduate", 
                                1, 0)
df_post$father_college <- ifelse(df_post$father_education == "Bachelor's degree" | df_post$father_education == "Postgraduate", 
                                1, 0)

df_post$parent_college <- ifelse(df_post$father_education == "Bachelor's degree" | df_post$father_education == "Postgraduate" | 
                                   df_post$mother_education == "Bachelor's degree" | df_post$mother_education == "Postgraduate", 
                                 1, 0)

df_parentedu <- df_post %>% group_by(State, School, Class) %>% summarise(parent_college = mean(parent_college))
df_condensed <- df_condensed %>%
  left_join(df_parentedu, by = c("State", "School", "Class")) 


#Write it for graphing
write_xlsx(df_condensed, "df_condensed.xlsx")

#See graphs for visual representation, non-sig linear relationship for total dataset, 
  #when random intercept for state then we see a significant effect
model1 <- glm(mean_score_post ~ mean_score_pre, data = df_condensed, weights = class_size)
model2 <- lmer(mean_score_post ~ mean_score_pre + (1 | State), data = df_condensed, weights = class_size)
summary(model1)
summary(model2)

#USE THIS ONE
model3 <- lmer(change_score ~ mean_score_pre + (1 | State), data = df_condensed, weights = class_size)
summary(model3)$r.squared
summ(model3, digits=5)
model_summary <- tidy(model3)
write_xlsx(list("Model Summary" = model_summary), "model_summary.xlsx")

tbl_regression(model3, exponentiate = TRUE)

model3v2 <- glm(change_score ~ mean_score_pre, data = df_condensed, weights = class_size)
ggplot(df_condensed, aes(x = mean_score_pre, y = change_score, size = class_size)) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Mean Score Pre", 
       y = "Change Score") + 
  theme_classic()

summary(model3v2)

#Now we ask if change in the score is predictive of change in vaccination status
#Will need to talk about these
model4 <- glm(change_vaccination_status ~ change_score, data = df_condensed, weights = class_size)
model5 <- lmer(change_vaccination_status ~ change_score + (1 | State), data = df_condensed, weights = class_size)
summary(model4)
summary(model5)
#Re-run model 4 with the -22% removed
model5v2 <- lmer(change_vaccination_status ~ change_score + (1 | State), 
               data = df_condensed %>% filter(change_score >= 0 & change_vaccination_status >= 0), 
               weights = class_size)
summary(model5v2)

model6 <- lmer(change_vaccination_status ~ change_perception + (1 | State), data = df_condensed, weights = class_size)
model6v2 <- lmer(change_vaccination_status ~ post_perception + (1 | State), data = df_condensed, weights = class_size)
  #model 6 significant p = 0.03, 6v2 p = 0.058
