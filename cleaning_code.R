#Analysis. Code by James Dickerson, MD MS and Nicole Kim, BS
options(scipen=999)
setwd("/Users/jamesdickerson/Library/CloudStorage/Box-Box/Dickerson Lab/Dickerson_Lab_Github/")

library(dplyr)
library(tidyverse)
library(readxl)

#Cleaning Code

FCT_baseline_1 <- read_excel("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_/FCT_Baseline_Responses_edited.xlsx",
                             sheet = 1)
FCT_baseline_1 <- FCT_baseline_1 %>%
  mutate(School = "Gov Jnr Sec Schl, Dutse")
FCT_baseline_2 <- read_excel("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_/FCT_Baseline_Responses_edited.xlsx",
                             sheet = 2)
FCT_baseline_2 <- FCT_baseline_2 %>%
  mutate(School = "Aleyita Jnr Sec Schl")

FCT_pre <- bind_rows(FCT_baseline_1, FCT_baseline_2)
colnames(FCT_pre)[c(11:39)] <- paste0("Pre_", colnames(FCT_pre)[c(11:39)])

FCT_endline_1 <- read_excel("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_/FCT_Endline_Responses_edited.xlsx", 
                            sheet = 1)
FCT_endline_1 <- FCT_endline_1 %>%
  mutate(School = "Gov Jnr Sec Schl, Dutse")
FCT_endline_2 <- read_excel("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_/FCT_Endline_Responses_edited.xlsx",
                            sheet = 2)
FCT_endline_2 <- FCT_endline_2 %>%
  mutate(School = "Aleyita Jnr Sec Schl")

FCT_post <- bind_rows(FCT_endline_1, FCT_endline_2)
colnames(FCT_post)[c(9:37)] <- paste0("Post_", colnames(FCT_post)[c(9:37)])

FCT <- bind_rows(FCT_pre, FCT_post)
FCT <- FCT %>%
  group_by("Class", "Age", "Religion", "Father's highest level of education", "Mother's highest level of education",
           "Father's occupation", "Mother's occupation", "State", "LGA", "School") 

