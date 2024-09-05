# load libraries
library(janitor)
library(readxl)
library(tidyverse)

# read in the file from data / mahadi
mahadi.df <- read_excel("data/mahadi/NREC Lysimeter Results.xlsx") |> clean_names()

# make a simple plot just of the ISU data
mahadi.df |> 
  filter(farm=="ISU") |>
  ggplot(aes(x=crop, y = porewater_no3_mgl, color = type)) +
  stat_summary(fun = mean, geom = "point", 
               shape = 23, size = 3, fill = "white",
               position= position_dodge(width = 0.2)
  ) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               width = 0.2, 
               position= position_dodge(width = 0.2)
  )+
  facet_grid(original_coll_date~.)+
  labs(title = "ISU Data",
       x = "Year",
       y = "Nitrate (mg/L)")

# lets make a quick table of the means by date, depth for nitrate
mahadi.df |> 
  filter(farm=="ISU") |>
  group_by(original_coll_date, crop, type) |> 
  summarise(mean_no3 = mean(porewater_no3_mgl, na.rm = TRUE)) |> 
  pivot_wider(names_from = type, values_from = mean_no3) |> 
  janitor::clean_names() |> 
  knitr::kable()
