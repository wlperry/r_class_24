# libraries
library(readxl)
library(janitor)
library(tidyverse)
library(patchwork)

# load data from maggie ----
m.df <- read_excel("data/Maddie/wlp_modified_Mouse_Trapping_Data.xlsx")

# to fix the manual NA values
m.df <- read_excel("data/Maddie/wlp_modified_Mouse_Trapping_Data.xlsx",
                   na="NA")

write_csv(m.df, "output/Maddie/mice.csv")

m.df <- m.df |> 
  mutate(island_size_category = case_when(
    islandsize < 7300 ~ "Small",
    islandsize >= 7300 & islandsize < 9050000 ~ "Medium",
    islandsize >= 9050000 ~ "Large",
    TRUE ~ "Other"))

m.df <- m.df |> 
  mutate(island_dist_category = case_when(
    distance < 5000 ~ "near",
    distance >= 5000 & distance < 7500 ~ "far",
    distance >= 7500 ~ "distant",
    TRUE ~ "Other"))



# BASIC PLOTS ------ -- -- - - - - -------- 
# so we have some data in here and we want to look at what it says by island
m.df |> 
  ggplot(aes(x=sampling_site, y=weight_total_grams, color=sampling_site))+
  geom_point()

# we could switch the x and y axes
m.df |> 
  ggplot(aes(y=sampling_site, x=weight_total_grams, color=sampling_site))+
  geom_point()

# we could also use coord_flip
m.df |> 
  ggplot(aes(x=sampling_site, y=weight_total_grams, color=sampling_site))+
  geom_point() + coord_flip()

# what if we wanted to see all the hidden points
m.df |> 
  ggplot(aes(y=sampling_site, x=weight_total_grams, color=sampling_site))+
  geom_point(position = position_dodge2(width = 0.3))

# Histogram PLOTS ------ -- -- - - - - -------- 
# what if you wanted a hisyogram of the weights
m.df |> 
  ggplot(aes(y=weight_total_grams, fill=sampling_site))+
  geom_histogram() 

# how to see all the sites individually
m.df |> 
  ggplot(aes(y=weight_total_grams, fill=sampling_site))+
  geom_histogram()  +
  facet_wrap(.~sampling_site)


# Mean and Standard error plots  - - - - - - - - - - - ------------------
# what if we wanted to see the mean and the standard error
m.df |> 
  ggplot(aes(y=sampling_site, x=weight_total_grams, color=sampling_site))+
  stat_summary(fun=mean, geom = "point")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2)

# so now we want to change the look and feel of the data
m.df |> 
  ggplot(aes(y=sampling_site, x=weight_total_grams, color=sampling_site))+
  stat_summary(fun=mean, geom = "point")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2)+
  theme_classic()

# we can also make our own theme 
# and we only change one place rather than each plot
# Run this and it will store it as an object for use later
theme_mice <- function(
    base_size = 14,  # really important to scale for big and small plots
    base_family = "sans") # sets the type of general font
          {theme(
            # panel stuff
            panel.grid.major = element_line(linetype = "blank"),
            panel.grid.minor = element_line(linetype = "blank"),
            panel.background = element_rect(fill = NA),
            # Axis stuff
            axis.line = element_line(linewidth  = 0.5, linetype = "solid"), 
            axis.ticks = element_line(colour = "black"),
            axis.title = element_text(size = 14, face = "bold"), 
            axis.text = element_text(colour = "black"),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(face = "bold"),
            # legend stuff
            legend.text = element_text(size = 12, face = "bold"), 
            legend.title = element_text(size = 13, face = "bold"), 
            legend.key = element_rect(fill = NA),
            legend.position = "right",
            legend.background = element_rect(fill = NA)
            )
          }

# now we can apply our theme
m.df |> 
  ggplot(aes(y=sampling_site, x=weight_total_grams, color=sampling_site))+
  stat_summary(fun=mean, geom = "point")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2)+
  theme_mice()

# we can also set up the scale color manual settings
# colors
# get the name
unique(m.df$sampling_site)

scale_color <- scale_color_manual(
  name = "Location",
  labels = c( "Mandarte Island"   = "Mandarte Island" ,   
              "Vancouver Island"  = "Vancouver Island" ,  
              "Sidney Island"     = "Sidney Island"   ,   
              "North Pender"      = "North Pender"     ,  
              "D'Arcy Island"     = "D'Arcy Island"   ,   
              "Saturna Island"    = "Saturna Island"   ,  
              "Vancouver"         = "Vancouver"       ,   
              "Saltspring Island" = "Saltspring Island",  
              "Portland Island"   = "Portland Island" ,   
              "South Pender"      = "South Pender"),
  values = c( "Mandarte Island"   = "black"      ,  
              "Vancouver Island"  = "blue"       ,  
              "Sidney Island"     = "goldenrod1" ,   
              "North Pender"      = "yellow4"    ,   
              "D'Arcy Island"     = "orange3"    ,   
              "Saturna Island"    = "purple"     ,  
              "Vancouver"         = "grey34"     ,  
              "Saltspring Island" = "red4"       ,  
              "Portland Island"   = "green4"    ,   
               "South Pender"     = "blue4" ),  drop = FALSE ) 


m.df |> 
  ggplot(aes(y=sampling_site, x=weight_total_grams, color=sampling_site))+
  stat_summary(fun=mean, geom = "point")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2)+
  scale_color+
  theme_mice()

# now lets make sampling_site a factor
m.df <- m.df |> 
  mutate(sampling_site = as.factor(sampling_site))


# now order the points by weight_total_grams in decreasing order for the samplign_site using forecats and fct_reorder decreasing
m.df |> 
  ggplot(aes(y=fct_reorder(sampling_site, weight_total_grams, .desc = FALSE), 
             x=weight_total_grams, color=sampling_site))+
  stat_summary(fun=mean, geom = "point")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2)+
  scale_color+
  theme_mice()

# what if we wanted to make a manaul order of factors....
m.df <- m.df |> 
  mutate(sampling_site = fct_relevel(sampling_site, 
            "Vancouver", "Saltspring Island","Vancouver Island",
            "North Pender","Sidney Island","Mandarte Island",
            "Saturna Island", "Portland Island",   
            "D'Arcy Island", "South Pender" ))

# Now the plot will be ordered but the factors above
m.df |> 
  ggplot(aes(y=sampling_site, 
             x=weight_total_grams, color=sampling_site))+
  stat_summary(fun=mean, geom = "point")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2)+
  scale_color+
  theme_mice()

# or we can order by another value 
m.df |> 
  ggplot(aes(y=fct_reorder(sampling_site, weight_total_grams, .desc = FALSE), 
             x=weight_total_grams, color=sampling_site))+
  stat_summary(fun=mean, geom = "point")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2)+
  scale_color+
  theme_mice()

m.df |> 
  ggplot(aes(y=fct_reorder(sampling_site, islandsize, .desc = TRUE, .na_rm = TRUE), 
             x=weight_total_grams, color=sampling_site))+
  stat_summary(fun=mean, geom = "point", na.rm=TRUE)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2)+
  scale_color+
  theme_mice()


# Ok Now how can we work with the datsframe to make changes to values
# note that we have weights total with bag and weight of bag but no mouse weight

# we can use mutate to calculate this
m.df <- m.df |> 
  mutate(mouse_wt_g = weight_total_grams - weight_bag_grams)

