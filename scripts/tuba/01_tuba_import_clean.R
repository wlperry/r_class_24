# # install packages if you have not done so... 
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("janitor")

# load libraries
library(janitor)
library(readxl)
library(tidyverse)
library(patchwork)


# read in data
tuba.df <- read_excel("data/tuba/1st harvest Pennycress VFA Data_wlp_modified.xlsx")


# save file - or write it to a new CSV file
write_csv(tuba.df, "output/tuba/example.csv")

ggplot(data=tuba.df, aes(x=crop, y=conc_ppm, fill=crop)) +
  geom_boxplot()
  
plot1.plot <- tuba.df |> 
ggplot( aes(crop, conc_ppm, fill=crop)) +
  geom_boxplot()
plot1.plot

plot2.plot <- tuba.df |> 
  ggplot( aes(crop, conc_ppm, fill=crop)) +
  geom_point()
plot2.plot

plot1.plot + plot2.plot

tuba.df |> 
  ggplot( aes(crop, conc_ppm, shape=crop, fill=crop)) +
  geom_point()


theme_hungary <- function(base_size = 24, base_family = "sans")
{theme(
  axis.line = element_line(size = 0.5, linetype = "solid"), 
  axis.ticks = element_line(colour = "black"),
  axis.title = element_text(size = 12, face = "bold"), 
  axis.text = element_text(colour = "black"),
  axis.text.x = element_text(size = 16),
  axis.text.y = element_text(size = 18),
  plot.title = element_text(face = "bold"),
  legend.text = element_text(size = 14, face = "bold"), 
  legend.title = element_text(size = 18, face = "bold"), 
  legend.key = element_rect(fill = NA),
  legend.position = "bottom",
  legend.background = element_rect(fill = NA),
  panel.grid.major = element_line(linetype = "blank"),
  panel.grid.minor = element_line(linetype = "blank"),
  panel.background = element_rect(fill = NA))
}


# colors
scale_color <- scale_color_manual(name = "Treatment",
                                  labels=c(CC = "Covercress", GPC="Golden Pennycress", WPC = "Wild Pennycress"),
                                  values = c(CC = "blue", GPC="green", WPC = "red"))


# Graph the data
final.plot <- tuba.df |> 
  ggplot(aes(x=crop, conc_ppm, shape=crop, color=crop))  +
  geom_point() +
  scale_color+
  theme_hungary()
final.plot

ggsave(final.plot, file="figures/final_plot.pdf", units = "in", 
       width = 26, height = 16)



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
  stat_summary(fun.data = mean_se, geom = "errorbar",
               width = 0.2,
               )+
  labs(title = "1st Harvest Pennycress VFA Data",
       x = "Crop",
       y = "Yield (g/g VS)")

# plot area vs height from the tuba.df ----
tuba.df |> 
  ggplot(aes(x=area, y = height, color=crop)) +
  geom_point() +
  labs(title = "1st Harvest Pennycress VFA Data",
       x = "Area (cm^2)",
       y = "Height (cm)")
