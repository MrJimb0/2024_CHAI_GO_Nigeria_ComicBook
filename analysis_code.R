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

setwd("/Users/jamesdickerson/Library/CloudStorage/Box-Box/Dickerson Lab/Dickerson_Lab_Github/2024_CHAI_GO_Nigeria_ComicBook/Data_/Updated files from Nicole 8:17:24")
#setwd("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook")

df_total <- read_xlsx("df_total_cleaned.xlsx")
df_pre <- read_xlsx("df_pre_cleaned.xlsx")
df_post <- read_xlsx("df_post_cleaned.xlsx")

#Summary statistics For Tables 
                      #@ Nicole [enter code here :) ]

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

#Now, we want to look at the relationship between parental education and pre-test score both for mom and dad 
              
              #@ Nicole [enter code here :) ] stratify by state and then graph in the graphing file



#Next, we do a bunch of t tests. All are significant with improvements in scores 
t_tests <- list(
  FCT = tidy(t.test(df_post[df_post$State == "FCT",]$survey_score, df_pre[df_pre$State == "FCT",]$survey_score)),
  Kaduna = tidy(t.test(df_post[df_post$State == "Kaduna",]$survey_score, df_pre[df_pre$State == "Kaduna",]$survey_score)),
  Rivers = tidy(t.test(df_post[df_post$State == "Rivers",]$survey_score, df_pre[df_pre$State == "Rivers",]$survey_score)),
  Lagos = tidy(t.test(df_post[df_post$State == "Lagos",]$survey_score, df_pre[df_pre$State == "Lagos",]$survey_score)),
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
#Output table
kable(results_df, digits = 2)

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

#Because we need to pivot this df, this ggplot is here
vax_numbers_long <- vax_numbers %>%
  pivot_longer(cols = c(vaccinated_pre, vaccinated_post), names_to = "time", values_to = "vaccinations")
write_xlsx(vax_numbers_long, "vax_numbers_long.xlsx")




#MODELS
#MODELS
#MODELS

#Mixed effects Models

#Because of the inconsistently in the classes being reported, will start with class but may change to school as the unit of collapse (see line 96)
pre_score_mean <- df_pre %>% group_by(State, School, Class) %>% summarise(mean_score_pre = mean(survey_score))
post_score_mean <- df_post %>% group_by(State,School,Class) %>% summarise(mean_score_post = mean(survey_score))
df_condensed <- merge(pre_score_mean, post_score_mean, by=c("State", "School", "Class"), all = TRUE)
df_pre_vaccination <- df_pre %>% group_by(State, School, Class) %>% summarise(pre_vaccination_status = mean(vaccination_status))
df_post_vaccination <- df_post %>% group_by(State, School, Class) %>% summarise(post_vaccination_status = mean(vaccination_status))
df_condensed <- merge(df_condensed, df_pre_vaccination, by=c("State", "School", "Class"), all = TRUE)
df_condensed <- merge(df_condensed, df_post_vaccination, by=c("State", "School", "Class"), all = TRUE)
df_condensed <- df_condensed %>%
  mutate(pre_vaccination_status = pre_vaccination_status * 100,
         post_vaccination_status = post_vaccination_status * 100,
         change_vaccination_status = post_vaccination_status - pre_vaccination_status)
df_condensed <- df_condensed %>%
  mutate(pre_vaccination_status = ifelse(State %in% c("Rivers", "Kaduna") & is.na(pre_vaccination_status), 0, pre_vaccination_status))
df_condensed <- df_condensed %>%
  mutate(change_score = mean_score_post - mean_score_pre)


write_xlsx(df_condensed, "df_condensed.xlsx")

#See graphs for visual representaiton, non-sig linear relationship for total dataset, when random intercept for state then we see a significant effect
model1 <- glm(mean_score_post ~ mean_score_pre, data = df_condensed)
model2 <- lmer(mean_score_post ~ mean_score_pre + (1 | State), data = df_condensed)
summary(model1)
summary(model2)

#Now we ask if change in the score is predictive of change in vaccination status
#Will need to talk about these
model3 <- glm(change_vaccination_status ~ change_score, data = df_condensed)
model4 <- lmer(change_vaccination_status ~ change_score + (1 | State), data = df_condensed)
summary(model3)
summary(model4)
#More

