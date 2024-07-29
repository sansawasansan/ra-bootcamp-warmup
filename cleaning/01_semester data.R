###############################################################################=
# 2024 RA bootcamp warm-up task-----
# 01_cleaning
# 01_semester data ----
# BOSTWICK, Valerie, Stefanie Fischer, and Matthew Lang. (2022). 
# "Semesters or Quarters? TheEffect of the Academic Calendar on Postsecondary Student Outcomes.” 
# American EconomicJournal: Economic Polic
# start:2024/07/29
# end:2024/07/29
###############################################################################=
#------------------------------------------------------------------------------=
# dataset:semester data ----
#------------------------------------------------------------------------------=
# preparation ----
rm(list=ls())
getwd()

# library
library(tidyverse);library(skimr);library(dplyr)
#------------------------------------------------------------------------------=
#01 import dataset-----
#  生データを読み込みなさい (semester_dummy_1.csv, semester_dummy_2.csv)
#------------------------------------------------------------------------------=
# semester data : semester_data_1.csv,semester_data_2.csv
# data/warmup training package/01_data/raw/semester_dummy

sem_dt_1 <- read.csv("data/warmup training package/01_data/raw/semester_dummy/semester_data_1.csv")
sem_dt_2 <- read.csv("data/warmup training package/01_data/raw/semester_dummy/semester_data_2.csv")


#------------------------------------------------------------------------------=
#02 change the column names ----
#  semester_dummy_1.csvについては、1行目を列名としなさい
#------------------------------------------------------------------------------=
sem_dt_1
colnames(sem_dt_1) <- sem_dt_1[1, ]
sem_dt_1 <- sem_dt_1[-1, ]
sem_dt_1

colnames(sem_dt_2) <- c("unitid","instnm","semester","quarter","year","Y")
sem_dt_2

#------------------------------------------------------------------------------=
#03 aggregate the datasets ----
#  2つのデータを適切に結合しなさい
#------------------------------------------------------------------------------=
skim(sem_dt_1) # 1-6列目までcharacter
skim(sem_dt_2) # 2列：char, 1,3-6列目:numeric
class(sem_dt_2$x2)

head(sem_dt_1)
sem_dt_1$unitid <- as.numeric(sem_dt_1$unitid)
sem_dt_1$semester <- as.numeric(sem_dt_1$semester)
sem_dt_1$quarter <- as.numeric(sem_dt_1$quarter)
sem_dt_1$year <- as.numeric(sem_dt_1$year)
sem_dt_1$Y <- as.numeric(sem_dt_1$Y)

sem_data <- bind_rows(sem_dt_1,sem_dt_2)
sem_data
#------------------------------------------------------------------------------=
#04 delete the column -----
#  ʼYʼ列を削除しなさい
#------------------------------------------------------------------------------=
sem_data <- sem_data[,1:5] # sem_data[,-6]でも可
sem_data 

#------------------------------------------------------------------------------=
#04 add the column  of year-----
#  semester制が導入された年の列を作成しなさい。
#------------------------------------------------------------------------------=
# start_yr
sem_data <- sem_data %>%
  group_by(unitid) %>%
  arrange(year) %>%
  mutate(prev_semester = lag(semester, default = 0),
         start_yr = ifelse(prev_semester == 0 & semester == 1, year, NA)) %>%
  fill(start_yr, .direction = "downup") %>%
  ungroup() %>%
  select(-prev_semester) %>% 
  arrange(unitid, year) # unitid 毎に並び替え
sem_data
#------------------------------------------------------------------------------=
#05 add the dummy column -----
#  5.を用いてsemester制導入後を示すダミー変数を作成しなさい
# 2001年にsemester制が導入された場合、1991~2000年は0, 2001年以降は1となる変数
#------------------------------------------------------------------------------=
# sem_dum
sem_data <- sem_data %>%
  mutate(sem_dum = ifelse(start_yr >= 1991 & start_yr <= 2000, 0, ifelse(start_yr >= 2001, 1, NA))) 
sem_data

save(sem_data,file="cleaning/data/sem_data.RData")
#
