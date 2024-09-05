# # install packages if you have not done so... 
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("janitor")

# load libraries
library(janitor)
library(readxl)
library(tidyverse)

# read in data
tuba.df <- read_excel("data/tuba/1st harvest Pennycress VFA Data_wlp_modified.xlsx")


# save file - or write it to a new CSV file
write_csv(tuba.df, "output/tuba/1st_harvest_pennycress_cleaned.csv")


# Graph the data
tuba.df |> 
  ggplot(aes(x=crop, y = yield_g_g_vs)) 


# add a geometry
tuba.df |> 
  ggplot(aes(x=crop, y = yield_g_g_vs)) +
  geom_boxplot()
  
# add another geometry
# add a geometry
tuba.df |> 
  ggplot(aes(x=crop, y = yield_g_g_vs)) +
  geom_boxplot()


# note that all the data for each VFA is as one and not separated
# lets add a color to the points 
# this is called mapping a color, shape, or fill to a variable
tuba.df |> 
  ggplot(aes(x=crop, y = yield_g_g_vs, color = vfa)) +
  geom_boxplot()

# add a title and new axis labels
tuba.df |> 
  ggplot(aes(x=crop, y = yield_g_g_vs, color = vfa)) +
  geom_boxplot() +
  labs(title = "1st Harvest Pennycress VFA Data",
       x = "Crop",
       y = "Yield (g/g VS)")

# what if we wanted to do the mean and standard error using stat_summary
tuba.df |> 
  ggplot(aes(x=crop, y = yield_g_g_vs, color=vfa)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", 
               shape = 23, size = 3, fill = "white"
               ) +
  stat_summary(fun.data = mean_se, geom = "errorbar"
               width = 0.2,
               )+
  labs(title = "1st Harvest Pennycress VFA Data",
       x = "Crop",
       y = "Yield (g/g VS)")

