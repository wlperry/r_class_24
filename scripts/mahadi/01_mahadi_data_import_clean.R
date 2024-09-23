# install.packages(("ggThemeAssist"))
# library(ggThemeAssist)

# load libraries
library(janitor)
library(readxl)
library(tidyverse)
library(patchwork)

# read in the file from data / mahadi
mahadi.df <- read_excel("data/mahadi/NREC Lysimeter Results.xlsx") |> clean_names()

mahadi.df <- mahadi.df |> 
  mutate(crop = if_else(crop=="FPC", "WPC", crop))
  
mahadi.df <-  mahadi.df |>
  mutate(type = as.factor(type),
         crop = as.factor(crop)) |> 
  mutate(type = fct_relevel(type, "S", "L")) |> 
  mutate(crop = fct_relevel(crop, "F", "WPC", "GPC", "AR", "CR", "PCRO" ))

# this will show the levels
levels(mahadi.df$crop)
  
# make a simple plot just of the ISU data
isu.plot<- mahadi.df |>
  filter(farm == "ISU") |>
  ggplot(aes(x = crop, y = porewater_no3_mgl, 
             group=1, fill=type,color = type)) +
  stat_summary(
    fun = mean, geom = "point", # does the mean as a point
    shape = 23, size = 5, #fill = "white",
    position = position_dodge(width = 0.5)
  ) +
  stat_summary(
    fun = mean, geom = "line", # mean as a line
    linewidth = 1, #fill = "white", # line width
    position = position_dodge(width = 0.5)
  ) +
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    linewidth = 1,
    width = 0.2, position = position_dodge(width = 0.5)
  )  +
  facet_grid(type ~ ., labeller = labeller(type = c(L = "Deep", S = "Shallow"))) + # doen as row and column
  labs(
    title = "ISU Data",
    x = "Year",
    y = "Nitrate-N (mg/L)"
  ) + 
  coord_cartesian(ylim=c(0,30))+
  scale_color_manual(name = "Lysimeter \nDepth",
                     label = c(L="Deep", S="Shallow"),
                     values = c(L="black", S="green"))+
  scale_fill_manual(name = "Lysimeter \nDepth",
                     label = c(L="Deep", S="Shallow"),
                     values = c(L="black", S="green"))+
  theme_classic()+
  theme(strip.background = element_blank())
isu.plot

wiu.plot<- mahadi.df |>
  filter(farm == "WIU") |>
  ggplot(aes(x = crop, y = porewater_no3_mgl, 
             group=1, fill=type,color = type)) +
  stat_summary(
    fun = mean, geom = "point",
    shape = 23, size = 5, #fill = "white",
    position = position_dodge(width = 0.5)
  ) +
  stat_summary(
    fun = mean, geom = "line",
    linewidth = 1, #fill = "white",
    position = position_dodge(width = 0.5)
  ) +
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    linewidth = 1,
    width = 0.2, position = position_dodge(width = 0.5)
  ) +
  coord_cartesian(ylim=c(0,30))+
  facet_grid(type ~ ., labeller = labeller(type = c(L = "Deep", S = "Shallow"))) + # doen as row and column
  labs(
    title = "WIU Data",
    x = "Year",
    y = "Nitrate-N (mg/L)"
  ) +
  scale_color_manual(name = "Lysimeter \nDepth",
                     label = c(L="Deep", S="Shallow"),
                     values = c(L="black", S="green"))+
  scale_fill_manual(name = "Lysimeter \nDepth",
                    label = c(L="Deep", S="Shallow"),
                    values = c(L="black", S="green"))+
  theme_classic()+
  theme(strip.background = element_blank())
wiu.plot
  
final.plot <-
  isu.plot + theme_classic(base_size = 26,)+ theme( strip.text = element_blank())+
  wiu.plot + theme_classic(base_size = 26,)+ theme( axis.title.y=element_blank(), axis.text.y=element_blank()) +
  plot_layout(ncol = 2, guides = "collect") +
  NULL
final.plot

ggsave(final.plot, file="figures/final_nitrate.pdf", units = "in", 
       width = 16, height = 6)

ggsave(final.plot, file="figures/final_nitrate.png", units = "in", 
       width = 16, height = 6)


# other way
mahadi.df |>
  # filter(farm == "WIU") |>
  ggplot(aes(x = crop, y = porewater_no3_mgl, 
             group=1, fill=type,color = type)) +
  stat_summary(
    fun = mean, geom = "point",
    shape = 23, size = 5, #fill = "white",
    position = position_dodge(width = 0.5)
  ) +
  stat_summary(
    fun = mean, geom = "line",
    linewidth = 1, #fill = "white",
    position = position_dodge(width = 0.5)
  ) +
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    linewidth = 1,
    width = 0.2, position = position_dodge(width = 0.5)
  ) +
  coord_cartesian(ylim=c(0,30))+
  facet_grid(type ~ farm, labeller = labeller(type = c(L = "Deep", S = "Shallow"))) + # doen as row and column
  labs(
    title = "Nitrate Data",
    x = "Year",
    y = "Nitrate-N (mg/L)"
  ) +
  scale_color_manual(name = "Lysimeter \nDepth",
                     label = c(L="Deep", S="Shallow"),
                     values = c(L="black", S="green"))+
  scale_fill_manual(name = "Lysimeter \nDepth",
                    label = c(L="Deep", S="Shallow"),
                    values = c(L="black", S="green"))+
  theme_classic()+
  theme(strip.background = element_blank())

# 
  # theme(axis.line = element_line(linetype = "solid"),
  #   axis.ticks = element_line(colour = "black"),
  #   panel.grid.major = element_line(linetype = "blank"),
  #   panel.grid.minor = element_line(linetype = "blank"),
  #   axis.title = element_text(size = 18,
  #       face = "bold"), axis.text = element_text(size = 12,
  #       face = "bold", colour = "black"),
  #   axis.text.x = element_text(size = 18,
  #       colour = "black"), axis.text.y = element_text(size = 12,
  #       colour = "black"), plot.title = element_text(size = 18,
  #       face = "bold"), legend.text = element_text(size = 12,
  #       face = "bold"), legend.title = element_text(size = 15,
  #       face = "bold"), panel.background = element_rect(fill = NA),
  #   legend.key = element_rect(fill = NA),
  #   legend.background = element_rect(fill = NA))

# lets make a quick table of the means by date, depth for nitrate
mahadi.df |> 
  filter(farm=="ISU") |>
  group_by(original_coll_date, crop, type) |> 
  summarise(mean_no3 = mean(porewater_no3_mgl, na.rm = TRUE)) |> 
  pivot_wider(names_from = type, values_from = mean_no3) |> 
  janitor::clean_names() |> 
  knitr::kable()
