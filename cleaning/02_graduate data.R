###############################################################################=
# 2024 RA bootcamp warm-up task-----
# 01_cleaning
# 02_graduate data ----
# BOSTWICK, Valerie, Stefanie Fischer, and Matthew Lang. (2022). 
# "Semesters or Quarters? TheEffect of the Academic Calendar on Postsecondary Student Outcomes.” 
# American EconomicJournal: Economic Polic
# start:2024/07/29
# end:2024/07/29
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

d_list <- list()
dataframe_names <- list.files(path = "./data/warmup training package/01_data/raw/outcome/", full.names = FALSE, pattern = "\\.xlsx$") %>%
  gsub(".xlsx$", "", .)

for (name in dataframe_names) {
  filepath <- paste("./data/warmup training package/01_data/raw/outcome/", name, ".xlsx", sep="")
  d_list[[name]] <- readxl::read_excel(filepath)  
}
d_list # outcome List data by year

#------------------------------------------------------------------------------=
#02 change the women_graduate_4yr rate----
# 女子学生の4年卒業率に0.01をかけて、0から1のスケールに変更しなさい
#------------------------------------------------------------------------------=
for(i in seq_along(d_list)){
  d_list[[i]]$women_gradrate_4yr <- d_list[[i]]$women_gradrate_4yr * 0.01
}
d_list

#------------------------------------------------------------------------------=
#03 add tot_gradrate_4yr rate the men_gradrate_4yr rate and 
# 男女合計の4年卒業率と男子学生の4年卒業率を計算し、新たな列として追加しなさい
#------------------------------------------------------------------------------=
for(i in seq_along(d_list)){
  d_list[[i]]$totcohortsize <- as.numeric(d_list[[i]]$totcohortsize)
  d_list[[i]]$tot_gradrate_4yr <- d_list[[i]]$tot4yrgrads/d_list[[i]]$totcohortsize 
  d_list[[i]]$m_4yrgrads <- as.numeric(d_list[[i]]$m_4yrgrads)
  d_list[[i]]$men_gradrate_4yr <- d_list[[i]]$m_4yrgrads/d_list[[i]]$m_cohortsize 
  }
d_list[[1]]
#------------------------------------------------------------------------------=
#04 change the women_graduate_4yr rate----
#計算した卒業率を有効数字3桁に調整しなさい
#------------------------------------------------------------------------------=
for(i in seq_along(d_list)){
  d_list[[i]]$tot_gradrate_4yr <- round(d_list[[i]]$tot_gradrate_4yr,3)
  d_list[[i]]$men_gradrate_4yr <- round(d_list[[i]]$men_gradrate_4yr,3)
  d_list[[i]]$women_gradrate_4yr <- round(d_list[[i]]$women_gradrate_4yr,3)
}
d_list[[1]]

#------------------------------------------------------------------------------=
#05 change time period to 1991-2010-----
# 1991年から2010年までのデータフレームに変形しなさい
#------------------------------------------------------------------------------=
d_list # data 1991-2016
grad_list <- d_list[1:19] #1991-2010
grad_list
save(grad_list,file="cleaning/data/grad_list.RData")
