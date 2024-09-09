# # install.packages
# install.packages("readxl")
# install.packages("janitor")
# install.packages("tidyverse")

# load libraries
library(tidyverse)
library(janitor)
library(readxl)

k.df <- read_excel("data/ISU Fall 2022 Soil test results wlp modified.xlsx") |>
  clean_names()

ggplot(k.df, aes(treatment, icp_k))+
  geom_boxplot()

k.df |> 
  filter(depth == "0-2") |> 
ggplot( aes(treatment, soil_carbon))+
  stat_summary(fun=mean, geom = "point")+
  stat_summary(fun.data = mean_se, geom = "errorbar")

t.df <- read_excel("data/1st harvest Pennycress VFA Data_wlp_modified.xlsx")
t.df <- t.df |> 
  mutate(undil_conc_ppm=conc_ppm*5)

total.df <- t.df |> 
  group_by(crop, sample) |> 
  summarize(total_vfa = sum(undil_conc_ppm)) |> 
  arrange(crop, sample)
# plot of total vfa
total.df |> 
  ggplot( aes(crop, total_vfa))+
  stat_summary(fun=mean, geom = "point")+
  stat_summary(fun.data = mean_se, geom = "errorbar")
# calculated yeild totals 
total_yield.df <- t.df |> 
  group_by(crop, sample) |> 
  summarize(total_yeild = sum(yield_g_g_vs)) |> 
  arrange(crop, sample)


total_yield.df |> 
  ggplot( aes(crop, total_yeild))+
  stat_summary(fun=mean, geom = "point")+
  stat_summary(fun.data = mean_se, geom = "errorbar")


t.df |> 
  ggplot(aes(crop, undil_conc_ppm, color=vfa))+
  stat_summary(fun=mean, geom = "point")+
  stat_summary(fun.data = mean_se, geom = "errorbar")+facet_wrap(~vfa, scales="free_y")
