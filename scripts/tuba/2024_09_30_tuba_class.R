
# libraries -----
library(readxl)
library(janitor)
library(tidyverse)


# read in the data form the data/tuba/ folder
tuba.df <- read_excel("data/tuba/1st harvest Pennycress VFA Data_wlp_modified.xlsx") |> 
  clean_names()

# so we can see the dataframe in line
glimpse(tuba.df)


# MUTATE -----------------------------------------------------------------
# now we want to redo her math - I dont trust excel...
# she did 
       # =G5*5/10^6*256/9.487
       # = conc_ppm * (5/10^6) * (256/9.487)

tuba.df <- tuba.df |> 
  mutate(yield_2 =   conc_ppm * (5/10^6) * (256/9.487)  )

# SELECT ------------------------------------------------------------------
# now if we want to remove columns that are not relevant
tuba.df <- tuba.df |> 
  select(-yield_2)

tuba_2.df <- tuba.df |> 
  select(starts_with("ret_"))

# FILTER -----------------------------------------------------------------
# what if we only wanted to show acetic acid

acetic_acid.df <- tuba.df |> 
  filter(vfa=="Acetic acid"   )

# and again if we wanted all the acids we could do 
acid.df <- tuba.df |> 
  filter( str_detect(crop, "C")    )

# SUMMARIZE ---------------------------
# Now we want to summarize our data
vfa_sum.df <- tuba.df |>
  group_by(crop, vfa) |> 
  summarize(mean_conc_ppm = mean(conc_ppm, na.rm=TRUE)         )

# add in SE
vfa_sum.df <- tuba.df |> 
  group_by(crop, vfa) |> 
  summarize(mean_conc_ppm = mean(conc_ppm, na.rm=TRUE),
            se_conc = sd(conc_ppm)/sum(!is.na(conc_ppm))^.5   )


# what if we wanted this by the vfa?
vfa_sum.df <- tuba.df |> 
  group_by(    ) |> 
  summarize(mean_conc_ppm = mean(conc_ppm, na.rm=TRUE),
            se_conc = sd(conc_ppm)/sum(!is.na(conc_ppm))^.5   )

# what if we wanted by crop and vfa
# what if we wanted this by the vfa?
vfa_sum.df <- tuba.df |> 
  group_by(crop, vfa) |> 
  summarize(mean_conc_ppm = mean(conc_ppm, na.rm=TRUE),
            se_conc = sd(conc_ppm)/sum(!is.na(conc_ppm))^.5   )

# we can also add in Standard errors for plotting
vfa_sum.df <- tuba.df |> 
  group_by(crop, vfa) |> 
  summarize(
    sample = first(sample),
    mean_conc_ppm = mean(conc_ppm, na.rm=TRUE),
            se_conc = sd(conc_ppm)/sum(!is.na(conc_ppm))^.5    )
# why use the code above and not n()?

# we can also plot this smaller dataframe
# Plotting the mean with SE bars
vfa_sum.df |> 
  ggplot(aes(x = vfa, y = mean_conc_ppm, fill=crop,color = crop)) +
  geom_bar(stat = "identity", 
             position = position_dodge2(width=0.3)) +  # Bar plot for means
  geom_errorbar(aes(ymin = mean_conc_ppm - se_conc, 
                    ymax = mean_conc_ppm + se_conc), 
                width = 0.3, 
                position = position_dodge2(width=0.5)) +  # Error bars for SE
  labs(title = "Mean VFA Conc by Treatment",
       x = "acid",
       y = "VFA Conc (PPM)") +
  theme_minimal()


