library(tidyverse)

taia <- read_csv("https://github.com/angelgardt/taia/raw/master/data/taia.csv")

str(taia)
colnames(taia)

taia %>% 
  select(starts_with(c("pr", "co", "ut", "fa", "de", "un", "gt")),
         age,
         sex,
         edulvl1,
         e_dighelp) %>% 
  mutate(PR = pr01 + pr02 + pr03 + pr04 + pr05 + pr06 + pr07 + pr08 + pr09 + pr10,
         CO = co01 + co02 + co03 + co04 + co05 + co06 + co08 + co09 + co10,
         UT = ut01 + ut02 + ut03 + ut04 + ut05 + ut06 + ut07 + ut08 + ut09 + ut11 + ut12,
         FA = fa01 + fa02 + fa03 + fa04 + fa05 + fa06 + fa07 + fa08 + fa09 + fa10,
         DE = de01 + de02 + de03 + de05 + de06 + de07 + de08 + de09 + de10 + de11,
         UN = un01 + un02 + un03 + un04 + un05 + un06 + un07 + un08 + un09 + un10 + un11 + un12,
         TAIA = PR + CO + UT + FA + DE + UN,
         GT = (gt01 + gt02 + gt03 + gt04 + gt05 + gt06) / 6,
         ) %>%
  write_csv("taia-sub.csv")

taia$edulvl1
