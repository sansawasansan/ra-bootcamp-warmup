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
# install.packages("pastecs")
# library
library(tidyverse);library(skimr);library(dplyr);library(pastecs);library(gt)

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

sum_stat <- stat.desc(sub) %>%
  as.data.frame() %>%
  rownames_to_column(var = "statistic") %>%
  pivot_longer(-statistic, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  select(variable, mean, SE.mean, std.dev, median, min, max, sum) %>%
  mutate(across(mean:sum, ~ round(., 1)))
sum_stat

unitid_count <- n_distinct(master_data$unitid)
unitid_row <- data.frame(
  variable = "unitid_count",
  mean = NA_real_,
  SE.mean = NA_real_,
  std.dev = NA_real_,
  median = NA_real_,
  min = NA_real_,
  max = NA_real_,
  sum = NA_real_,
  count = unitid_count
)

sum_stat <- bind_rows(unitid_row, sum_stat)

gt_table <- sum_stat %>% gt() %>%
  tab_header(title = "Summary Table") %>%
  cols_label(
    variable = "Variable",
    mean = "Mean",
    SE.mean = "SE Mean",
    std.dev = "Std Dev",
    median = "Median",
    min = "Min",
    max = "Max",
    sum = "Sum"
  )
gt_table


