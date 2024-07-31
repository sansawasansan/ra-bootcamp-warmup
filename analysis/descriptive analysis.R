###############################################################################=
# 2024 RA bootcamp warm-up task-----
# 02_analysis
# 01 descripitive analysis----
# BOSTWICK, Valerie, Stefanie Fischer, and Matthew Lang. (2022). 
# "Semesters or Quarters? TheEffect of the Academic Calendar on Postsecondary Student Outcomes.” 
# American EconomicJournal: Economic Polic
# start:2024/07/30
# end:
###############################################################################=
#------------------------------------------------------------------------------=
#01 descriptive analysis----
# dataset:master_data 
#------------------------------------------------------------------------------=
#preparation
rm(list=ls())
install.packages("psych")
# library
library(tidyverse);library(skimr);library(dplyr);library(psych);library(gt)

#library(tidyr)



# import data set
load(file="cleaning/data/master_data.RData")# data frame
master_data
#------------------------------------------------------------------------------=
#01_1 check the missing values
#「(d) Master Dataの作成」で作成したデータの、各列に含まれるNAの数を数えなさい。
#------------------------------------------------------------------------------=
skim(master_data)
# women_gradrate_4yr に24個のNA
# men_gradrate_4yr  に65個のNA　

# women_gradrate_4yr または men_gradrate_4yr にNAが含まれる行を抽出
na_rows <- master_data %>%
  filter(is.na(women_gradrate_4yr) | is.na(men_gradrate_4yr))
print(na_rows,n=89)

# 男女ともにcohorotとgradsの人数が0のため、rateを0とあらわさず、NAとなっていた。
# NAとなっている値をすべてrate=0に置き換え

master_data <- master_data %>%
  mutate(women_gradrate_4yr = ifelse(is.na(women_gradrate_4yr), 0, women_gradrate_4yr),
         men_gradrate_4yr = ifelse(is.na(men_gradrate_4yr), 0, men_gradrate_4yr))
master_data
skim(master_data) # すべての行でNAなし


#------------------------------------------------------------------------------=
#01_2 make the descriptive summary table ----
# 記述統計表を作りなさい
#------------------------------------------------------------------------------=
summary(master_data)

sub <- master_data %>% select(totcohortsize,w_cohortsize,m_cohortsize, tot4yrgrads,m_4yrgrads,tot_gradrate_4yr,men_gradrate_4yr,women_gradrate_4yr,instatetuition,costs,faculty,white_cohortsize )
sub
# ?describe
sum_stat <- describe(sub) %>% as.data.frame() %>% 
  select( n, mean, sd, median, min, max, se) %>%
  mutate(across(everything(), ~ round(., 1))) %>% 
  rownames_to_column(var = "variable")
sum_stat
gt(sum_stat) %>% tab_header(title="summary table")
# unit_id 足したい