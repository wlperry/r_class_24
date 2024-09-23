
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
) 


scale_color_manual(name = "Lysimeter \nDepth",
                   label = c(L="Deep", S="Shallow"),
                   values = c(L="black", S="green"))


mutate(type = as.factor(type)) |> 
  mutate(type = fct_relevel(type, "S", "L")) 


theme_midsize <- function(base_size = 14, base_family = "Sans")
{
  theme(
    # LABLES APPEARANCE
    axis.text = element_text(colour = "black"),
    axis.title.x=element_text(size=18, face="bold"),
    axis.title.y=element_text(size=18, face="bold"),
    axis.text.x = element_text(size=16, face="bold", angle=0, vjust = .5, hjust=.5),
    axis.text.y = element_text(size=16, face="bold"),
    # plot.title = element_text(hjust = 0.5, colour="black", size=22, face="bold"),
    # LEGEND
    # legend.position="bottom",
    # LEGEND TEXT
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    # LEGEND TITLE
    legend.title = element_text(colour="black", size=18, face="bold"),
    # LEGEND POSITION AND JUSTIFICATION 
    # legend.justification=c(0.1,1),
    legend.position="bottom",
    # PLOT COLORS
    # REMOVE BOX BEHIND LEGEND SYMBOLS
    # REMOVE LEGEND BOX
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    # REMOVE LEGEND BOX
    legend.background = element_rect(fill = "transparent", colour = "transparent"),
    # #REMOVE PLOT FILL AND GRIDS
    panel.background=element_rect(fill = "transparent", colour = "transparent"), 
    plot.background=element_rect(fill="transparent", colour=NA),
    # # removes the grid lines
    panel.grid.major = element_line(linetype = "blank"),
    panel.grid.minor = element_line(linetype = "blank"),
    # ADD AXES LINES AND SIZE
    axis.ticks = element_line(colour = "black"),
    axis.line.x = element_line(color="black", size = 0.5,linetype = "solid" ),
    axis.line.y = element_line(color="black", size = 0.5, linetype = "solid"),
    # ADD PLOT BOX
    panel.border = element_rect(colour = "black", fill = NA, size=2),
    plot.title = element_text(hjust = 0, vjust=2.12),
    plot.caption = element_text(hjust = 0, vjust=1.12))
}


