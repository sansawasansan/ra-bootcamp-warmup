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
