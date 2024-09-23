# load libraries ----
library(tidyverse)
library(readxl)
library(janitor)
library(skimr)
library(lubridate)
library(patchwork)

# read in files ----
# this is how you read a csv file - commas separated values
# this file - reduced_mutant_seed_parameters.csv - should be in your data directory
isu.df  <- read_excel("data/keion/ISU Fall 2022 Soil test results wlp modified.xlsx", 
                            sheet="ISU 1-216") %>% 
  clean_names() |> 
  mutate(site = "isu") |> 
  mutate(soil_carbon = as.numeric(soil_carbon),
        no3 = as.numeric(no3),
        nh4 = as.numeric(nh4),
        p = as.numeric(p),
        icp_k = as.numeric(icp_k))

wiu.df <- read_excel("data/keion/ISU Fall 2022 Soil test results wlp modified.xlsx", 
                            sheet="WIU 1-216") %>% 
  clean_names() |> 
  mutate(site = "wiu")|> 
  mutate(soil_carbon = as.numeric(soil_carbon),
        no3 = as.numeric(no3),
        nh4 = as.numeric(nh4),
        p = as.numeric(p),
        icp_k = as.numeric(icp_k))

# bind all the data by row together----
soc.df <- bind_rows(isu.df , wiu.df)

# clean data here for later -----
unique(soc.df$depth)

soc.df <- soc.df %>% 
  mutate(depth_cm = case_when(
    depth == "0-2"    ~ "0"    ,
    depth == "2-4"    ~ "2"    ,
    depth == "4-6"    ~ "4"    ,
    depth == "6-8"    ~ "6"    ,
    depth == "8-10"   ~ "8"    ,
    depth == "10-15"  ~ "10 "  ,
    depth == "15-20"  ~ "15 "  ,
    depth == "20-25"  ~ "20 "  ,
    depth == "25-30"  ~ "25 "  ,
    TRUE ~ "other"
  )) |> 
  mutate(depth_cm = as.numeric(depth_cm))

write_csv(soc.df, "output/keion/soil_carbon.csv")



