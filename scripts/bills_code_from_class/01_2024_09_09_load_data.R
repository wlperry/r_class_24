# install pachkages
# installed.packages("tidyverse")


# load libraries 
library(readxl)
library(janitor)
library(tidyverse)

# load data
mahadi.df <- read_excel("data/keion/ISU Fall 2022 Soil test results wlp modified.xlsx",
                        na="NA") |> 
  clean_names()

write_csv(mahadi.df , file="output/test.csv")


no3.plot <-mahadi.df |> 
  ggplot( aes(treatment, no3, colour = block))+
  geom_boxplot()+
  geom_point()+
  # labs(y="Nitrate-N (ppm)",
  #      x="Treattment")
 NULL
no3.plot

ggsave(plot.plot, file="plot.pdf", units="in", width=6, height=6)
