###############################################################################=
# 2024 RA bootcamp warm-up task-----
# 01_cleaning
# 02_graduate data ----
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
# graduate data: outcome(1991-2016).xlsx
# data/warmup training package/01_data/raw/outcome

dataframe_list <- list()
dataframe_names <- list.files(path = "./data/warmup training package/01_data/raw/outcome/", full.names = FALSE, pattern = "\\.xlsx$") %>%
  gsub(".xlsx$", "", .)

for (name in dataframe_names) {
  filepath <- paste("./data/warmup training package/01_data/raw/outcome/", name, ".xlsx", sep="")
  dataframe_list[[name]] <- readxl::read_excel(filepath)  
}
dataframe_list # outcome List data by year

#------------------------------------------------------------------------------=
#02 change the women_graduate_4yr rate----
# 女子学生の4年卒業率に0.01をかけて、0から1のスケールに変更しなさい
#------------------------------------------------------------------------------=
for(i in seq_along(dataframe_list)){
  dataframe_list[[i]]$women_gradrate_4yr <- dataframe_list[[i]]$women_gradrate_4yr * 0.01
}
dataframe_list

#------------------------------------------------------------------------------=
#03 add tot_gradrate_4yr rate the men_gradrate_4yr rate and 
# 男女合計の4年卒業率と男子学生の4年卒業率を計算し、新たな列として追加しなさい
#------------------------------------------------------------------------------=






#------------------------------------------------------------------------------=