# load libraries -----
library(tidyverse)
library(readxl)
library(janitor)
library(skimr)
library(lubridate)
library(patchwork)
library(plotly)

# read in the file -----
soc.df <- read_csv("output/keion/soil_carbon.csv")

# Make factors -----
soc.df <-  soc.df %>% 
  mutate(
    block = as.factor(block),
    depth = as.factor(depth),
    treatment = as.factor(treatment))

# what is the order of factors ------
levels(soc.df$treatment)

# reorder factors -----
soc.df <-  soc.df %>% 
  mutate(treatment = fct_relevel(treatment, 
                    c("Control", "Ann Ryegrass", "Cereal Rye", "Wild PC", "Golden PC", "PCRO")))

# what is the new order of factors ----
levels(soc.df$treatment)


levels(soc.df$depth)
# reorder factors
soc.df <-  soc.df %>% 
  mutate(depth = fct_relevel(depth, 
          c("0-2", "2-4","4-6", "6-8", "8-10",  "10-15", "15-20", "20-25", "25-30" )))

# Set up graphs so its less code throughout
# Theme for Graphs
# Make a new default theme
# Run this and it will store it as an object for use later
theme_soc <- function(base_size = 14, base_family = "Times")
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
scale_color <- scale_color_manual(
  name = "Treatment",
  labels = c( "Control"= "Control", "Ann Ryegrass" = "Ann Ryegrass", 
              "Cereal Rye" = "Cereal Rye", "Wild PC" = "Wild PC", 
              "Golden PC" = "Golden PC", "PCRO" = "PCRO"),
  values = c( "Control"= "brown4", "Ann Ryegrass" ="darkgreen", "Cereal Rye" ="green4", 
              "Wild PC" ="black" ,  "Golden PC" = "goldenrod3", 
              "PCRO" = "blue3"), drop = FALSE ) 
  

# fill
scale_fill <- scale_fill_manual(
  name = "Treatment",
  labels = c( "Control"= "Control", "Ann Ryegrass" = "Ann Ryegrass", 
              "Cereal Rye" = "Cereal Rye", "Wild PC" = "Wild PC", 
              "Golden PC" = "Golden PC", "PCRO" = "PCRO"),
  values = c( "Control"= "brown4", "Ann Ryegrass" ="darkgreen", 
              "Cereal Rye" ="green4", "Wild PC" ="black" ,  "Golden PC" = "goldenrod3", 
              "PCRO" = "blue3"),
  drop = FALSE ) 

# shape
scale_shape <- scale_shape_manual(
  name = "Treatment",
  labels = c( "Control"= "Control", "Ann Ryegrass" = "Ann Ryegrass", 
              "Cereal Rye" = "Cereal Rye", "Wild PC" = "Wild PC", 
              "Golden PC" = "Golden PC", "PCRO" = "PCRO"),
  values = c("Control"= 25, "Ann Ryegrass" = 21,  "Cereal Rye" = 22, 
             "Wild PC" =23 ,"Golden PC" = 23, "PCRO" = 24),
  drop = FALSE  )


# ISU Graph
isu.plot <- soc.df %>%
  filter(site=="isu") %>%
  mutate(treatment = fct_rev((treatment))) |>
  ggplot(aes(x=depth_cm, y=soil_carbon,  
             shape = treatment, color=treatment, fill=treatment)) +
  stat_summary(
    fun=mean, na.rm = TRUE,
    geom = "bar",
    position = position_dodge2(width=.6 )) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE,
    geom = "errorbar",
    # width=3,
    position = position_dodge2(width=.6)
    ) +
  scale_x_reverse( breaks=seq(0,26,2), expand = c(0,0)) +
  # scale_x_reverse()+
  scale_y_continuous(position = "right", expand = c(0,0)) +
  coord_flip() +
  labs(y="Soil Organic Matter\n(% LOI)", x="Depth (cm)" ) + 
  scale_color+
  scale_fill+
  scale_shape +
  theme_soc() + 
  # facet_grid(.~treatment)+
  NULL
isu.plot


ggsave(isu.plot, file="figures/soil organic carbon.pdf", 
       width = 10, height = 7, units = "in")

isu_category.plot <- soc.df %>%
  mutate(depth = fct_rev(factor(depth))) %>%
  mutate(treatment = fct_rev(factor(treatment))) %>%
  filter(site=="isu") %>%
  ggplot(aes(x=depth, y=soil_carbon, 
             shape = treatment, color=treatment, fill=treatment)) +
  stat_summary(
    fun=mean, na.rm = TRUE,
    geom = "bar",
    # size=3,
    position = position_dodge2(width=0.7)
  ) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE,
    geom = "errorbar",
    width=.9,
    position = position_dodge2(width=.7)
  ) +
  scale_y_continuous(position = "right", expand = c(0,0)) +
  scale_x_discrete( expand = c(0,0)) +
  coord_flip() +
  labs(y="Soil Organic Matter\n(% LOI)", x="Depth (cm)" ) + 
  scale_color+
  scale_fill+
  scale_shape +
  theme_soc() + 
  # facet_grid(.~treatment)+
  NULL
isu_category.plot

ggsave(isu_category.plot, file="figures/keion/soil organic carbon as categories.pdf", 
       width = 10, height = 7, units = "in")


wiu_category.plot <- soc.df %>%
  mutate(depth = fct_rev(factor(depth))) %>%
  mutate(treatment = fct_rev(factor(treatment))) %>%
  filter(site=="wiu") %>%
  ggplot(aes(x=depth, y=soil_carbon, 
             shape = treatment, color=treatment, fill=treatment)) +
  stat_summary(
    fun=mean, na.rm = TRUE,
    geom = "bar",
    # size=3,
    position = position_dodge2(width=0.7)
  ) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE,
    geom = "errorbar",
    width=.9,
    position = position_dodge2(width=.7)
  ) +
  scale_y_continuous(position = "right", expand = c(0,0)) +
  scale_x_discrete( expand = c(0,0)) +
  coord_flip() +
  labs(y="Soil Organic Matter\n(% LOI)", x="Depth (cm)" ) + 
  scale_color+
  scale_fill+
  scale_shape +
  theme_soc() + 
  # facet_grid(.~treatment)+
  NULL
wiu_category.plot


isu_no3_category.plot <- soc.df %>%
  mutate(depth = fct_rev(factor(depth))) %>%
  mutate(treatment = fct_rev(factor(treatment))) %>%
  filter(site=="isu") %>%
  ggplot(aes(x=depth, y=no3, 
             shape = treatment, color=treatment, fill=treatment)) +
  stat_summary(
    fun=mean, na.rm = TRUE,
    geom = "bar",
    # size=3,
    position = position_dodge2(width=0.7)
  ) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE,
    geom = "errorbar",
    width=.9,
    position = position_dodge2(width=.7)
  ) +
  scale_y_continuous(position = "right", expand = c(0,0)) +
  scale_x_discrete( expand = c(0,0)) +
  coord_flip() +
  labs(y="Nitrate -N (PPM)", x="Depth (cm)" ) + 
  scale_color+
  scale_fill+
  scale_shape +
  theme_soc() + 
  # facet_grid(.~treatment)+
  NULL
isu_no3_category.plot


isu_p_category.plot <- soc.df %>%
  mutate(depth = fct_rev(factor(depth))) %>%
  mutate(treatment = fct_rev(factor(treatment))) %>%
  filter(site=="isu") %>%
  ggplot(aes(x=depth, y=p, 
             shape = treatment, color=treatment, fill=treatment)) +
  stat_summary(
    fun=mean, na.rm = TRUE,
    geom = "bar",
    # size=3,
    position = position_dodge2(width=0.7)
  ) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE,
    geom = "errorbar",
    width=.9,
    position = position_dodge2(width=.7)
  ) +
  scale_y_continuous(position = "right", expand = c(0,0)) +
  scale_x_discrete( expand = c(0,0)) +
  coord_flip() +
  labs(y="Phosphorous (PPM)", x="Depth (cm)" ) + 
  scale_color+
  scale_fill+
  scale_shape +
  theme_soc() + 
  # facet_grid(.~treatment)+
  NULL
isu_p_category.plot

isu_no3_category.plot +
  isu_p_category.plot +
  plot_layout( ) #guides = "collect"



soc_category.plot <- soc.df %>%
  mutate(depth = fct_rev(factor(depth))) %>%
  mutate(treatment = fct_rev(factor(treatment))) %>%
  # filter(site=="isu") %>%
  ggplot(aes(x=depth, y=soil_carbon, 
             shape = treatment, color=treatment, fill=treatment)) +
  stat_summary(
    fun=mean, na.rm = TRUE,
    geom = "bar",
    # size=3,
    position = position_dodge2(width=0.7)
  ) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE,
    geom = "errorbar",
    width=.9,
    position = position_dodge2(width=.7)
  ) +
  scale_y_continuous(position = "right", expand = c(0,0)) +
  scale_x_discrete( expand = c(0,0)) +
  coord_flip() +
  labs(y="Soil Organic Matter\n(% LOI)", x="Depth (cm)" ) + 
  scale_color+
  scale_fill+
  scale_shape +
  theme_soc() + 
  facet_grid(.~site)+
  NULL
soc_category.plot


# make the soc.df to long format 
soc_long.df <- soc.df %>%
  pivot_longer(cols = c(soil_carbon, no3, nh4, p, icp_k), 
               names_to = "variable", values_to = "value")

test.plot <- soc_long.df %>%
  mutate(depth = fct_rev(factor(depth))) %>%
  mutate(treatment = fct_rev(factor(treatment))) %>%
  # filter(site=="isu") %>%
  ggplot(aes(x=depth, y=value, 
             shape = treatment, color=treatment, fill=treatment)) +
  stat_summary(
    fun=mean, na.rm = TRUE,
    geom = "bar",
    # size=3,
    position = position_dodge2(width=0.7)
  ) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE,
    geom = "errorbar",
    width=.9,
    position = position_dodge2(width=.7)
  ) +
  scale_y_continuous(position = "right", expand = c(0,0)) +
  scale_x_discrete( expand = c(0,0)) +
  coord_flip() +
  labs(y="Soil Organic Matter\n(% LOI)", x="Depth (cm)" ) + 
  scale_color+
  scale_fill+
  scale_shape +
  theme_soc() + 
  facet_grid(site~variable, scales = "free_x")+
  NULL
test.plot


# trying something to look at differences from control
soc_diff.df <- soc.df %>%
  group_by(site, block, depth) %>%
  mutate(control_value = soil_carbon[treatment == "Control"]) %>%
  mutate(soil_carbon_diff = soil_carbon - control_value) |> 
  filter(treatment != "Control")

isu_soc_diff.plot <- soc_diff.df %>%
  filter(site=="isu") %>%
  mutate(treatment = fct_rev((treatment))) |>
  ggplot(aes(x=depth_cm, y=soil_carbon_diff,  
             shape = treatment, color=treatment, fill=treatment)) +
  stat_summary(
    fun=mean, na.rm = TRUE,
    geom = "bar",
    position = position_dodge2(width=.6 )) +
  stat_summary(
    fun.data = mean_se, na.rm = TRUE,
    geom = "errorbar",
    # width=3,
    position = position_dodge2(width=.6)
  ) +
  scale_x_reverse( breaks=seq(0,26,2), expand = c(0,0)) +
  # scale_x_reverse()+
  scale_y_continuous(position = "right", expand = c(0,0)) +
  coord_flip() +
  labs(y="Difference in Soil Organic Matter\n(% LOI (treatment- control))", x="Depth (cm)" ) + 
  scale_color+
  scale_fill+
  scale_shape +
  theme_soc() + 
  # facet_grid(.~treatment)+
  NULL
isu_soc_diff.plot


ggsave(isu.plot, file="figures/soil organic carbon.pdf", 
       width = 10, height = 7, units = "in")

