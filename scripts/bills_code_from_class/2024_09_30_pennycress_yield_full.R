
# libraries -----
library(readxl)
library(janitor)
library(tidyverse)


# read in the data form the edata/perrry/ folder
yield.df <- read_excel("data/perry_pc/pennycress_yeild_2024.xlsx") |> 
  clean_names()

# so we can see the dataframe in line
glimpse(yield.df)


# MUTATE -----------------------------------------------------------------
# now we want to take the pc_yield_lbs and convert it to lb/acre
yield.df <- yield.df |> 
  mutate(yield_lb_acre = pc_yield_lbs * to_convert_acre_mutlby)

# we could have also put in the conversion factor which is 
# 174
yield.df <- yield.df |> 
  mutate(yield_lb_acre = pc_yield_lbs * 174)

# and to get to kg/acre
yield.df <- yield.df |> 
  mutate(yield_kg_acre = yield_lb_acre * .4536)

# to get to bushels per acre we divide by 50lb/bu - test weight of lbs/bushel
yield.df <- yield.df |> 
  mutate(yield_bsh_acre = (yield_lb_acre / 50)) 


# SELECT ------------------------------------------------------------------
# now if we want to remove columns that are not relevant if we were sending out final data
yield_short.df <- yield.df |> 
  select(plot, treatment, yield_lb_acre, yield_kg_acre)

# we can also do a differnt way
yield_short_2.df <- yield.df |> 
  select(-c(pc_yield_lbs, pc_moisture, notes, yield_bsh_acre, to_convert_acre_mutlby))

# can also use 
# starts_with""
# ends_with""
# so retain only the _n
yield_short_3.df <- yield.df |> 
  select(plot, treatment, ends_with("_acre"))
 
# super helpful if you designed your variable names with preplanning.......


# FILTER -----------------------------------------------------------------
# what if we only wanted to show a guest the pc data and no +N
yield_pc_only.df <- yield_short_2.df |> 
  filter(treatment=="pc")

# and again we could do another way
yield_pc_only_2.df <- yield_short_2.df |> 
  filter(treatment == "pc_n")

# there are other ways to filter  - we cn use a stringer command - 
     # will get to this more
yield_pc_only_3.df <- yield_short_2.df |> 
filter(!str_detect(treatment, "_n"))
# note changing the ! will change which is useful and subtle


# SUMMARIZE ---------------------------
# Now we want to summarize our data
yield_sum.df <- yield.df |> 
  summarize(mean = mean(yield_lb_acre, na.rm=TRUE))

# we can add in standard error
yield_sum.df <- yield.df |> 
  summarize(mean = mean(yield_lb_acre, na.rm=TRUE),
            se = sd(yield_lb_acre), sum(!is.na(yield_lb_acre)))
# why use the code above and not n()?

# now we want to know the yeld by the two treaments
yield_sum.df <- yield.df |> 
  group_by(treatment) |> 
  summarize(mean = mean(yield_lb_acre, na.rm=TRUE),
            se = sd(yield_lb_acre), sum(!is.na(yield_lb_acre)))

# now thats not really fair as we ahve 4 reps per plot and need to average that first 
# and then take the average of the treatments
yield_sum.df <- yield.df |> 
  group_by(treatment, plot) |> 
  summarize(mean = mean(yield_lb_acre, na.rm=TRUE),
            se = sd(yield_lb_acre), sum(!is.na(yield_lb_acre)))

yield_sum.df <- yield_sum.df |> 
  group_by(treatment) |> 
  summarize(mean = mean(mean, na.rm=TRUE),
            se = sd(mean), sum(!is.na(mean)))


# now we can do the code above

# we can also plot this smaller dataframe
# Plotting the mean with SE bars
yield_sum.df |> 
  ggplot(aes(x = treatment, y = mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Bar plot for means
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +  # Error bars for SE
  labs(title = "Mean Yield with Standard Error",
       x = "Treatment",
       y = "Yield (lb/acre)") +
  theme_minimal()


