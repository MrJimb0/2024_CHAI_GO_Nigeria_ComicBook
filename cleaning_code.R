#Cleaning Code.
#Authors: Nicole Kim, BS and James Dickerson, MD MS 

options(scipen=999)
#setwd("/Users/jamesdickerson/Library/CloudStorage/Box-Box/Dickerson Lab/Dickerson_Lab_Github/2024_CHAI_GO_Nigeria_ComicBook/Data_/Updated files from Nicole 8:17:24")
setwd("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_") 

library(tidyverse)
library(readxl)
library(writexl)

#Import the raw data from excel and format into a pre- and post- DF
#files were edited in excel to remove empty rows 

#Pre-intervention responses df
sheet_fctpre = excel_sheets("FCT_Baseline_Responses_edited.xlsx") 
FCT_pre = lapply(setNames(sheet_fctpre, sheet_fctpre),  
                    function(x1) read_excel("FCT_Baseline_Responses_edited.xlsx", sheet=x1)) 
FCT_pre = bind_rows(FCT_pre, .id="School") 

sheet_kadunapre = excel_sheets("Kaduna_Baseline_Responses_edited.xlsx") 
kaduna_pre = lapply(setNames(sheet_kadunapre, sheet_kadunapre),  
                    function(x2) read_excel("Kaduna_Baseline_Responses_edited.xlsx", sheet=x2)) 
kaduna_pre = bind_rows(kaduna_pre, .id="School") 

sheet_lagospre = excel_sheets("Lagos_Baseline_Responses_edited.xlsx") 
lagos_pre = lapply(setNames(sheet_lagospre, sheet_lagospre),  
                    function(x) read_excel("Lagos_Baseline_Responses_edited.xlsx", sheet=x)) 
lagos_pre = bind_rows(lagos_pre, .id="School") 

sheet_riverspre = excel_sheets("Rivers_Baseline_Responses_edited.xlsx") 
rivers_pre = lapply(setNames(sheet_riverspre, sheet_riverspre),  
                   function(x) read_excel("Rivers_Baseline_Responses_edited.xlsx", sheet=x)) 
rivers_pre = bind_rows(rivers_pre, .id="School") 

df_pre = bind_rows(FCT_pre, kaduna_pre, lagos_pre, rivers_pre)

#post-intervention responses df 
sheet_fctpost = excel_sheets("FCT_Endline_Responses_edited.xlsx") 
FCT_post = lapply(setNames(sheet_fctpost, sheet_fctpost),  
                    function(x) read_excel("FCT_Endline_Responses_edited.xlsx", sheet=x)) 
FCT_post = bind_rows(FCT_post, .id="School") 

sheet_kadunapost = excel_sheets("Kaduna_Endline_Responses_edited.xlsx") 
kaduna_post = lapply(setNames(sheet_kadunapost, sheet_kadunapost),  
                    function(x) read_excel("Kaduna_Endline_Responses_edited.xlsx", sheet=x)) 
kaduna_post = bind_rows(kaduna_post, .id="School") 

sheet_lagospost = excel_sheets("Lagos_Endline_Responses_edited.xlsx") 
lagos_post = lapply(setNames(sheet_lagospost, sheet_lagospost),  
                    function(x) read_excel("Lagos_Endline_Responses_edited.xlsx", sheet=x)) 
lagos_post = bind_rows(lagos_post, .id="School") 

sheet_riverspost = excel_sheets("Rivers_Baseline_Responses_edited.xlsx") 
rivers_post = lapply(setNames(sheet_riverspost, sheet_riverspost),  
                    function(x) read_excel("Rivers_Endline_Responses_edited.xlsx", sheet=x)) 
rivers_post = bind_rows(rivers_post, .id="School") 

df_post = bind_rows(FCT_post, kaduna_post, lagos_post, rivers_post)


#Standardization Code for variables

df_pre <- df_pre %>% 
  mutate(Student_ID = as.numeric(`Student #`)) %>% 
  dplyr::select(-`Student #`)
df_pre <- df_pre %>% 
  mutate(Age = as.numeric(str_extract(Age, "\\d+")))
df_pre <- df_pre %>% 
  mutate(Class = as.numeric(str_extract(Class, "\\d+")))

df_post <- df_post %>% 
  mutate(Student_ID = as.numeric(`Student #`)) %>% 
  dplyr::select(-`Student #`)
df_post <- df_post %>% 
  mutate(Age = as.numeric(str_extract(Age, "\\d+")))
df_post <- df_post %>% 
  mutate(Class = as.numeric(str_extract(Class, "\\d+")))

df_pre$student_confidence [df_pre$"If asked to speak to your friends about cervical cancer and the HPV vaccine at the school assembly, how would you rate yourself on a scale of 1-5?" == "1 - Not confident"] = 1
df_pre$student_confidence [df_pre$"If asked to speak to your friends about cervical cancer and the HPV vaccine at the school assembly, how would you rate yourself on a scale of 1-5?" == "2 - Slightly confident"] = 2
df_pre$student_confidence [df_pre$"If asked to speak to your friends about cervical cancer and the HPV vaccine at the school assembly, how would you rate yourself on a scale of 1-5?" == "3 - Somewhat confident"] = 3
df_pre$student_confidence [df_pre$"If asked to speak to your friends about cervical cancer and the HPV vaccine at the school assembly, how would you rate yourself on a scale of 1-5?" == "4 - Fairly confident"] = 4
df_pre$student_confidence [df_pre$"If asked to speak to your friends about cervical cancer and the HPV vaccine at the school assembly, how would you rate yourself on a scale of 1-5?" == "5 - Very confident"] = 5

df_post$student_confidence [df_post$"If asked to speak to your friends about cervical cancer and the HPV vaccine at the school assembly, how would you rate yourself on a scale of 1-5?" == "1 - Not confident"] = 1
df_post$student_confidence [df_post$"If asked to speak to your friends about cervical cancer and the HPV vaccine at the school assembly, how would you rate yourself on a scale of 1-5?" == "2 - Slightly confident"] = 2
df_post$student_confidence [df_post$"If asked to speak to your friends about cervical cancer and the HPV vaccine at the school assembly, how would you rate yourself on a scale of 1-5?" == "3 - Somewhat confident"] = 3
df_post$student_confidence [df_post$"If asked to speak to your friends about cervical cancer and the HPV vaccine at the school assembly, how would you rate yourself on a scale of 1-5?" == "4 - Fairly confident"] = 4
df_post$student_confidence [df_post$"If asked to speak to your friends about cervical cancer and the HPV vaccine at the school assembly, how would you rate yourself on a scale of 1-5?" == "5 - Very confident"] = 5


#df_total = merge(df_pre, df_post, by = c("Class", "Age", "Religion", "Father's highest level of education", "Mother's highest level of education",
  #                                       "Father's occupation", "Mother's occupation", "State", "LGA", "School"), all = TRUE)

#get student survey scores 
df_pre$Q1 <- +(df_pre$"What role do white blood cells play in the immune system?" == "Fighting off what makes you sick")
df_pre$Q2 <- +(df_pre$"Have you heard of the human papillomavirus (HPV)?" == "Yes")
df_pre$Q3 <- +(df_pre$"What is HPV?" == "A virus")
df_pre$Q4 <- +(df_pre$"Is HPV a virus that can cause cervical cancer?" == "Yes")
df_pre$Q5 <- +(df_pre$"Who is at risk of cervical cancer" == "Female")
df_pre$Q6 <- +(df_pre$"Which age group is the target for the HPV vaccine in Nigeria?" == "9-14 years")
df_pre$Q7 <- +(df_pre$"Which of the following cannot cause cancer?" == "Bad juju")
df_pre$Q8 <- +(df_pre$"Do you think it's safe to get vaccinated?" == "Yes")
df_pre$Q9 <- +(df_pre$"Is HPV screening/testing required even if you have been vaccinated against HPV?" == "Yes")
df_pre$Q10 <- +(df_pre$"Can you talk to your friends about cervical cancer and HPV?" == "Yes")
df_pre$Q11 <- +(df_pre$"Can you be an advocate for cervical cancer and the HPV vaccine?" == "Yes")
df_pre$Q12a <- +(df_pre$"You can get vaccinated with HPV at: Health centre" == "Yes")
df_pre$Q12b <- +(df_pre$"You can get vaccinated with HPV at: School" == "Yes")
df_pre$Q12c <- +(df_pre$"You can get vaccinated with HPV at: Mobile vaccination units" == "Yes")
df_pre$Q12d <- +(df_pre$"You can get vaccinated with HPV at: Religious homes" == "Yes")
df_pre$Q12e <- +(df_pre$"You can get vaccinated with HPV at: All of the above" == "Yes")

df_pre$survey_score <- rowSums(df_pre[, c("Q1", "Q3", "Q4", "Q5", "Q6", "Q7", "Q9")], na.rm=T)


df_post$Q1 <- +(df_post$"What role do white blood cells play in the immune system?" == "Fighting off what makes you sick")
df_post$Q2 <- +(df_post$"Have you heard of the human papillomavirus (HPV)?" == "Yes")
df_post$Q3 <- +(df_post$"What is HPV?" == "A virus")
df_post$Q4 <- +(df_post$"Is HPV a virus that can cause cervical cancer?" == "Yes")
df_post$Q5 <- +(df_post$"Who is at risk of cervical cancer" == "Female")
df_post$Q6 <- +(df_post$"Which age group is the target for the HPV vaccine in Nigeria?" == "9-14 years")
df_post$Q7 <- +(df_post$"Which of the following cannot cause cancer?" == "Bad juju")
df_post$Q8 <- +(df_post$"Do you think it's safe to get vaccinated?" == "Yes")
df_post$Q9 <- +(df_post$"Is HPV screening/testing required even if you have been vaccinated against HPV?" == "Yes")
df_post$Q10 <- +(df_post$"Can you talk to your friends about cervical cancer and HPV?" == "Yes")
df_post$Q11 <- +(df_post$"Can you be an advocate for cervical cancer and the HPV vaccine?" == "Yes")
df_post$Q12a <- +(df_post$"You can get vaccinated with HPV at: Health centre" == "Yes")
df_post$Q12b <- +(df_post$"You can get vaccinated with HPV at: School" == "Yes")
df_post$Q12c <- +(df_post$"You can get vaccinated with HPV at: Mobile vaccination units" == "Yes")
df_post$Q12d <- +(df_post$"You can get vaccinated with HPV at: Religious homes" == "Yes")
df_post$Q12e <- +(df_post$"You can get vaccinated with HPV at: All of the above" == "Yes")

df_post$survey_score <- rowSums(df_post[, c("Q1", "Q3", "Q4", "Q5", "Q6", "Q7", "Q9")], na.rm=T)

df_pre$heard_of_HPV <- +(df_pre$"Have you heard of the human papillomavirus (HPV)?" == "Yes")
df_post$heard_of_HPV <- +(df_post$"Have you heard of the human papillomavirus (HPV)?" == "Yes")

df_pre$vaccination_status <- +(df_pre$"Have you received the HPV vaccine?" == "Yes")
df_post$vaccination_status <- +(df_post$"Have you received the HPV vaccine?" == "Yes")

df_pre$perception <- +(df_pre$"Do you think it's safe to get vaccinated?" == "Yes")
df_post$perception <- +(df_post$"Do you think it's safe to get vaccinated?" == "Yes")

df_pre <- df_pre %>% 
  rename(
    father_education = "Father's highest level of education",
    mother_education = "Mother's highest level of education", 
    father_occupation = "Father's occupation",
    mother_occupation = "Mother's occupation")

df_post <- df_post %>% 
  rename(
    father_education = "Father's highest level of education",
    mother_education = "Mother's highest level of education", 
    father_occupation = "Father's occupation",
    mother_occupation = "Mother's occupation")


# Save dfs as excel files to wd  
write_xlsx(df_post, "df_post_cleaned.xlsx")
write_xlsx(df_pre, "df_pre_cleaned.xlsx")
