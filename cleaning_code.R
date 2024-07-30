#Analysis. Code by James Dickerson, MD MS and Nicole Kim, BS
options(scipen=999)
setwd("/Users/jamesdickerson/Library/CloudStorage/Box-Box/Dickerson Lab/Dickerson_Lab_Github/")

library(dplyr)
library(tidyverse)
library(readxl)

#Cleaning Code

setwd("/Users/nicolek/Desktop/GitHub/2024_CHAI_GO_Nigeria_ComicBook/Data_") 

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

df_total = merge(df_pre, df_post, by.x=c("Class", "Age", "Religion", "Father's highest level of education", "Mother's highest level of education",
                              "Father's occupation", "Mother's occupation", "State", "LGA", "School"), 
      by.y=c("Class", "Age", "Religion", "Father's highest level of education", "Mother's highest level of education",
             "Father's occupation", "Mother's occupation", "State", "LGA", "School"))

colnames(df_pre)[c(11:39)] <- paste0("Pre_", colnames(df_pre)[c(11:39)])
colnames(df_post)[c(9:37)] <- paste0("Post_", colnames(df_post)[c(9:37)])
df_total <- bind_rows(df_pre, df_post)

df_total <- df_total %>%
  group_by("Class", "Age", "Religion", "Father's highest level of education", "Mother's highest level of education",
           "Father's occupation", "Mother's occupation", "State", "LGA", "School") 


