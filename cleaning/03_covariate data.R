###############################################################################=
# 2024 RA bootcamp warm-up task-----
# 01_cleaning
# 03_Covariates Data----
# BOSTWICK, Valerie, Stefanie Fischer, and Matthew Lang. (2022). 
# "Semesters or Quarters? TheEffect of the Academic Calendar on Postsecondary Student Outcomes.” 
# American EconomicJournal: Economic Polic
# start:2024/07/29
# end:
###############################################################################=
#------------------------------------------------------------------------------=
# dataset:graduate data ----
#------------------------------------------------------------------------------=
# preparation ----
rm(list=ls())
getwd()

# library
library(tidyverse)
#------------------------------------------------------------------------------=
#01 import dataset-----
#------------------------------------------------------------------------------=
# covariates data: covariates.xlsx
# data/warmup training package/01_data/raw/covariates

cov_data <- readxl::read_excel("./data/warmup training package/01_data/raw/covariates/covariates.xlsx")
#------------------------------------------------------------------------------=
#02 change column names-----
# ʼuniversity_idʼという列名をʼunitidʼに変更しなさい
#------------------------------------------------------------------------------=

names(cov_data) <- c("unitid","year","category","value")
cov_data
#------------------------------------------------------------------------------=
#03 change the values-----
# ʼunitidʼに含まれる”aaaa”という文字を削除しなさい
#------------------------------------------------------------------------------=
cov_data <- cov_data %>%
  mutate(unitid = str_replace(unitid, "aaaa", ""))
cov_data

#------------------------------------------------------------------------------=
#04 add new columns ----
# ‘categoryʼ列に含まれるʼinstatetuitionʼ, ‘costsʼ, ʼfacultyʼ, ʼwhite_cohortsizeʼを別の列として追加しなさい
#-----------------------------------------------------------------------=
cov_data_wi<- cov_data %>%
  pivot_wider(names_from = category, values_from = value)

#------------------------------------------------------------------------------=
#05 arrange the data period-----
# outcomeやsemester_dummyに含まれる年を調べ、covariatesデータの期間を他のデータに揃えなさい
#------------------------------------------------------------------------------=
load(file="cleaning/data/sem_data.RData")
load(file="cleaning/data/grad_data.RData")

library(skimr)
skim(sem_data) #1991-2010
skim(grad_data) #1991-2010
skim(cov_data_wi) #variable types are categorical. need to change it to num

cov_data_wi <- cov_data_wi %>%
  mutate(across(everything(), as.numeric))
skim(cov_data_wi) # 1987-2016

# change the data period to 1991-2010
cov_data_wi <- cov_data_wi %>% 
  filter(year<=2010 & year>=1991)
skim(cov_data_wi) 

#------------------------------------------------------------------------------=
#05 arrange variables-----
# outcome_dataに含まれるunitidを特定し、covariatesに含まれるunitidをoutcomeデータに揃えなさい
#------------------------------------------------------------------------------=
grad_data$unitid
cov_data_wi$unitid
sem_data$unitid
