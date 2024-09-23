# this is an annotatyed script that we went through in class on 2024-09-16

# load libraries ----
library(janitor)
library(readxl)
library(tidyverse)

# read file and clean names ----
b.df <- read_excel("data/perry_pc/pennycress_yeild_2024.xlsx") |> 
  clean_names()

# plot data as a box plot
# This will pipe the dataframe into the ggplot command and produce a plot 
# we have not added in any geoms of how to plot the data so nothing will be there
b.df |> 
  ggplot(aes(treatment, pc_yield_lbs))

# this code is the same with no pipe but you will see why we do it the way above
ggplot(b.df, aes(treatment, pc_yield_lbs))

# now we can add a geom which can be a bar, point, line boxbplot or a number of other things
# try doing other geoms to see what is there
b.df |> 
  ggplot(aes(treatment, pc_yield_lbs)) +
  geom_boxplot()


# so ggplots are built in layers and we can add the actual datapoints on top opf the boxplot
b.df |> 
  ggplot(aes(treatment, pc_yield_lbs)) +
  geom_boxplot() +
  geom_point()


# note how the points are overlapping each other
# we can jitter or doge the points to see all of them - critical with lots of data
# there is sometime you need position_dodge and others you need position_dodge2 - not fully sure why
b.df |> 
  ggplot(aes(treatment, pc_yield_lbs)) +
  geom_boxplot() +
  geom_point(position = position_dodge2(width = 0.2))

# what if we want to change the colors of the points - we can map color onto a variable
# in this case we will do treatment
b.df |> 
  ggplot(aes(treatment, pc_yield_lbs, color=treatment)) +
  geom_boxplot() +
  geom_point(position = position_dodge2(width = 0.2))

# note if we do this for plot he results are unexpected as it is a continuous variable
b.df |> 
  ggplot(aes(treatment, pc_yield_lbs, color=plot)) +
  geom_boxplot() +
  geom_point(position = position_dodge2(width = 0.2))

# we need to change it to a categorical factor and we cna do in two ways....
# In the plot
b.df |> 
  ggplot(aes(treatment, pc_yield_lbs, color=as.factor(plot))) +
  geom_boxplot() +
  geom_point(position = position_dodge2(width = 0.2))

# In the data frame 
b.df <- b.df |> 
  mutate(plot = as.factor(plot))

# then plot as usual
b.df |> 
  ggplot(aes(treatment, pc_yield_lbs, color=plot)) +
  geom_boxplot() +
  geom_point(position = position_dodge2(width = 0.2))

# try this for fill as well.
# now e can do for shape as well... but this would be for the points alone so we can do
b.df |> 
  ggplot(aes(treatment, pc_yield_lbs, color=treatment)) +
  geom_boxplot() +
  geom_point(aes(shape=treatment),
    position = position_dodge2(width = 0.2))


# now what if we wnated to do colors that were special for the points
# and change the size
b.df |> 
  ggplot(aes(treatment, pc_yield_lbs, color=treatment)) +
  geom_point(
    position = position_dodge2(width = 0.2), size = 3) + # note putting size out of the aes() means it is changing size with no mapping
    scale_color_manual(
      name = "Fertilizer Treatment",
      label = c(pc = "No Nitrogen", `pc+n` = "Nitrogen added"),
      values = c(pc = "brown", `pc+n` = "darkgreen")
    ) 

# we can do the same for shape
b.df |> 
  ggplot(aes(treatment, pc_yield_lbs, color=treatment, shape=treatment)) +
  geom_point(
    position = position_dodge2(width = 0.2), size = 3) + # note putting size out of the aes() means it is changing size with no mapping
    scale_color_manual(
      name = "Fertilizer Treatment",
      label = c(pc = "No Nitrogen", `pc+n` = "Nitrogen added"),
      values = c(pc = "brown", `pc+n` = "darkgreen")
    ) +
      scale_shape_manual(
        name = "Fertilizer Treatment",
        label = c(pc = "No Nitrogen", `pc+n` = "Nitrogen added"),
        values = c(pc = 21, `pc+n` = 24)
      ) 

# we could also do fill if we use the shapes above
# https://www.datanovia.com/en/blog/ggplot-point-shapes-best-tips/
b.df |> 
  ggplot(aes(treatment, pc_yield_lbs, color=treatment, shape=treatment, fill = treatment)) +
  geom_point(
    position = position_dodge2(width = 0.2), size = 3) + # note putting size out of the aes() means it is changing size with no mapping
    scale_color_manual(
      name = "Fertilizer Treatment",
      label = c(pc = "No Nitrogen", `pc+n` = "Nitrogen added"),
      values = c(pc = "brown", `pc+n` = "darkgreen")
    ) +
      scale_shape_manual(
        name = "Fertilizer Treatment",
        label = c(pc = "No Nitrogen", `pc+n` = "Nitrogen added"),
        values = c(pc = 21, `pc+n` = 24)
      ) +
      scale_fill_manual(
          name = "Fertilizer Treatment",
          label = c(pc = "No Nitrogen", `pc+n` = "Nitrogen added"),
          values = c(pc = "grey", `pc+n` = NA)
     ) 



# what if we wanted to do a mean and standard error plot
b.df |> 
  ggplot(aes(treatment, pc_yield_lbs, color=treatment, shape=treatment, fill = treatment)) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    stat_summary(
        fun.data = "mean_se", 
        geom = "errorbar",
        width = .3, linewidth = 1) +
scale_color_manual(
      name = "Fertilizer Treatment",
      label = c(pc = "No Nitrogen", `pc+n` = "Nitrogen added"),
      values = c(pc = "brown", `pc+n` = "darkgreen")
    ) +
      scale_shape_manual(
        name = "Fertilizer Treatment",
        label = c(pc = "No Nitrogen", `pc+n` = "Nitrogen added"),
        values = c(pc = 21, `pc+n` = 24)
      ) +
      scale_fill_manual(
          name = "Fertilizer Treatment",
          label = c(pc = "No Nitrogen", `pc+n` = "Nitrogen added"),
          values = c(pc = "grey", `pc+n` = NA)
     ) 
  
  
# we can also change the look of the plot by adding a theme
# what if we wanted to do a mean and standard error plot
b.df |> 
  ggplot(aes(treatment, pc_yield_lbs, color=treatment, shape=treatment, fill = treatment)) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    stat_summary(
        fun.data = "mean_se", 
        geom = "errorbar",
        width = .3, linewidth = 1) +
scale_color_manual(
      name = "Fertilizer Treatment",
      label = c(pc = "No Nitrogen", `pc+n` = "Nitrogen added"),
      values = c(pc = "brown", `pc+n` = "darkgreen")
    ) +
      scale_shape_manual(
        name = "Fertilizer Treatment",
        label = c(pc = "No Nitrogen", `pc+n` = "Nitrogen added"),
        values = c(pc = 21, `pc+n` = 24)
      ) +
      scale_fill_manual(
          name = "Fertilizer Treatment",
          label = c(pc = "No Nitrogen", `pc+n` = "Nitrogen added"),
          values = c(pc = "grey", `pc+n` = NA)
     ) +
  theme_light()

# now to change the labels and then save the plto
b.df |> 
  ggplot(aes(treatment, pc_yield_lbs, color=treatment, shape=treatment, fill = treatment)) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    stat_summary(
        fun.data = "mean_se", 
        geom = "errorbar",
        width = .3, linewidth = 1) +
scale_color_manual(
      name = "Fertilizer Treatment",
      label = c(pc = "No Nitrogen", `pc+n` = "Nitrogen added"),
      values = c(pc = "brown", `pc+n` = "darkgreen")
    ) +
      scale_shape_manual(
        name = "Fertilizer Treatment",
        label = c(pc = "No Nitrogen", `pc+n` = "Nitrogen added"),
        values = c(pc = 21, `pc+n` = 24)
      ) +
      scale_fill_manual(
          name = "Fertilizer Treatment",
          label = c(pc = "No Nitrogen", `pc+n` = "Nitrogen added"),
          values = c(pc = "grey", `pc+n` = NA)
     ) +
  labs(
    x="Treatment", y="Yield (mean pounds per 5*50' +/- SE"
  )+
  theme_light()


# to save it you need to name the plot and store in the environment
yield.plot <- b.df |> 
  ggplot(aes(treatment, pc_yield_lbs, color=treatment, shape=treatment, fill = treatment)) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    stat_summary(
        fun.data = "mean_se", 
        geom = "errorbar",
        width = .3, linewidth = 1) +
scale_color_manual(
      name = "Fertilizer Treatment",
      label = c(pc = "No Nitrogen", `pc+n` = "Nitrogen added"),
      values = c(pc = "brown", `pc+n` = "darkgreen")
    ) +
      scale_shape_manual(
        name = "Fertilizer Treatment",
        label = c(pc = "No Nitrogen", `pc+n` = "Nitrogen added"),
        values = c(pc = 21, `pc+n` = 24)
      ) +
      scale_fill_manual(
          name = "Fertilizer Treatment",
          label = c(pc = "No Nitrogen", `pc+n` = "Nitrogen added"),
          values = c(pc = "grey", `pc+n` = NA)
     ) +
  labs(
    x="Treatment", y="Yield (mean pounds per 5*50' +/- SE"
  )+
  theme_light()
# then call the plot up to see it
yield.plot

# now save it
ggsave(yield.plot, file="figures/yield.pdf", units= "in", width = 6, height = 6)
