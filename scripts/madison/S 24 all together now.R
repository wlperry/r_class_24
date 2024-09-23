library(readxl)
library(tidyverse)
library(janitor)
library(ggpubr)
library(scales)
library(agricolae)
library(fitdistrplus)
library(DHARMa)
library(lme4)
library(car)
library(emmeans)
library(gamlss)

setwd("C:/Users/mbwilk1/OneDrive - IL State University/Summer 2024")
males <- read_excel("S'24 Hormone study qPCR mastersheet.xlsx")

oneway.test(ExactStage ~ ExactTreatment, data = males, var.equal = TRUE)

oneway.test(ExactStage ~ ExactTreatment, data = males, var.equal = FALSE)

males_aov <- aov(ExactStage ~ ExactTreatment * Day, data = males)
summary(males_aov)

plot(males_aov)
hist(males_aov$residuals)
shapiro.test(males_aov$residuals)

dazlposthoc <- HSD.test(dazlmodel, trt = "Treatment$Stage", group = TRUE)
TukeyHSD(dazlmodel)

ggplot(alltogether)+
  aes(x = Day, y = ExactStage, color = ExactTreatment) +
  stat_summary(fun=mean, na.rm = TRUE, 
             geom="line",
             size = 1.25,
             position = position_dodge(0.2)) +
  stat_summary(fun=mean, na.rm = TRUE, 
               geom="point",
               size = 1,
               position = position_dodge(0.2)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2,
               linetype="solid",
               position = position_dodge(0.2)) +
  labs(x = "Day", y = "Stage", 
       colour = "ExactTreatment")+
  scale_y_continuous(labels = comma) + theme(axis.line = element_line(linetype = "solid"), 
                                             panel.background = element_rect(fill = NA), 
                                             strip.text = element_text(colour = NA), 
                                             legend.key = element_rect(fill = NA), 
                                             legend.background = element_rect(fill = NA))

scale_color_manual(name = "ExactTreatment", 
                   values = c("#D977C6", "lightpink", "#485FF7", "lightblue"),
                   labels = c("FPT", "FPT + Estrogen", "MPT", "MPT + Estrogen")) +
#______________________
#make things factors
#DAZLtrunk$Stage <- as.factor(DAZLtrunk$AvgStage)
#DAZLtrunk$Temp <- as.factor(DAZLtrunk$ExactTreatment)
#summary(DAZLtrunk)


dazltrunkmodel<-aov(((DAZLtrunk$`DAZLNE`)^(.5))~ DAZLtrunk$Temp * DAZLtrunk$Stage, data=DAZLtrunk)
summary(dazltrunkmodel)

#Results from 8/8/24
#                               Df   Sum Sq   Mean Sq F value   Pr(>F)    
#DAZLtrunk$Temp                  3 0.000407 0.0001357   4.345 0.006603 ** 
#DAZLtrunk$Stage                 8 0.004802 0.0006002  19.220  < 2e-16 ***
#DAZLtrunk$Temp:DAZLtrunk$Stage  2 0.000497 0.0002487   7.964 0.000654 ***
#Residuals                      90 0.002811 0.0000312                                
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

par(mfrow=c(2,2))
plot(dazltrunkmodel)
hist(resid(dazltrunkmodel))

plotdist(DAZLtrunk$DAZLNE, histo = TRUE, demp = TRUE)
descdist(DAZLtrunk$DAZLNE, discrete = FALSE, boot = 500)

dazlposthoc <- HSD.test(dazltrunkmodel, trt = "Temp$Stage", group = TRUE)
TukeyHSD(dazltrunkmodel)

DAZLtrunk_long.df <- DAZLtrunk %>% 
  pivot_longer(
    cols = c(`DAZLNE`, `Cyp19NE`),
    names_to = "Gene",
    values_to = "Expression"
  )

y_ju <- expression(paste("Mean Normalized ", italic("DAZL"), " expression (+/- SE)"))
# ^ I have to do this to only make part of the axis label italic

DAZLtrunk_long.df %>% 
  filter(Gene == "DAZLNE") %>% 
  ggplot(aes(x=AvgStage, y=Expression, color=Temp, group=Temp)) +
  stat_summary(fun=mean, na.rm = TRUE, 
               geom="line",
               size = 1.25,
               position = position_dodge(0.2)) +
  stat_summary(fun=mean, na.rm = TRUE, 
               geom="point",
               size = 1,
               position = position_dodge(0.2)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2,
               linetype="solid",
               position = position_dodge(0.2)) +
  scale_color_manual(name = "Treatment", 
                     values = c("#D977C6", "lightpink", "#485FF7", "lightblue"),
                     labels = c("FPT", "FPT + Estrogen", "MPT", "MPT + Estrogen")) +
  labs(x = "Developmental Stage", y = y_ju, 
       colour = "Treatment")+
  scale_y_continuous(labels = comma) + theme(axis.line = element_line(linetype = "solid"), 
                                             panel.background = element_rect(fill = NA), 
                                             strip.text = element_text(colour = NA), 
                                             legend.key = element_rect(fill = NA), 
                                             legend.background = element_rect(fill = NA))

----
  # now to try cyp19 so I am copying from above and pasting it here and for some reason it keeps skipping back to the previous line so I'm going to paste the setwd line onwards here too
  
  #set working directory to where data file is
  setwd("C:/Users/mbwilk1/OneDrive - IL State University/Summer 2024")
Cyp19trunk <- read_excel("S'24 Hormone study qPCR mastersheet.xlsx")

#make things factors
Cyp19trunk$Stage <- as.factor(Cyp19trunk$AvgStage)
Cyp19trunk$Temp <- as.factor(Cyp19trunk$ExactTreatment)
summary(Cyp19trunk)

CYP19model<-aov(((Cyp19trunk$`Cyp19NE`)^(.5))~ Cyp19trunk$Temp * Cyp19trunk$Stage, data=Cyp19trunk)
summary(CYP19model)

#Results from 8/8/24
#                                 Df    Sum Sq   Mean Sq F value  Pr(>F)   
#Cyp19trunk$Temp                   3 0.0001278 4.261e-05   3.414   0.0208 *  
#Cyp19trunk$Stage                  8 0.0015743 1.968e-04  15.764 2.73e-14 ***
#Cyp19trunk$Temp:Cyp19trunk$Stage  2 0.0000198 9.920e-06   0.794   0.4550    
#Residuals                        90 0.0011235 1.248e-05                     
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

par(mfrow=c(2,2))
plot(CYP19model)
hist(resid(CYP19model))

plotdist(Cyp19trunk$Cyp19NE, histo = TRUE, demp = TRUE)
descdist(Cyp19trunk$Cyp19NE, discrete = FALSE, boot = 500)

Cyp19posthoc <- HSD.test(CYP19model, trt = "Temp$Stage", group = TRUE)
TukeyHSD(CYP19model)

Cyp19trunk_long.df <- Cyp19trunk %>% 
  pivot_longer(
    cols = c(`DAZLNE`, `Cyp19NE`),
    names_to = "Gene",
    values_to = "Expression"
  )

y_ju <- expression(paste("Mean Normalized ", italic("Cyp19"), " expression (+/- SE)"))
# ^ I have to do this to only make part of the axis label italic

Cyp19trunk_long.df %>% 
  filter(Gene == "Cyp19NE") %>% 
  ggplot(aes(x=AvgStage, y=Expression, color=Temp, group=Temp)) +
  stat_summary(fun=mean, na.rm = TRUE, 
               geom="line",
               size = 1.25,
               position = position_dodge(0.2)) +
  stat_summary(fun=mean, na.rm = TRUE, 
               geom="point",
               size = 1,
               position = position_dodge(0.2)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE,
               geom = "errorbar",
               width = 0.2,
               linetype="solid",
               position = position_dodge(0.2)) +
  scale_color_manual(name = "Treatment", 
                     values = c("#D977C6", "lightpink", "#485FF7", "lightblue"),
                     labels = c("FPT", "FPT + Estrogen", "MPT", "MPT + Estrogen")) +
  labs(x = "Developmental Stage", y = y_ju, 
       colour = "Treatment")+
  scale_y_continuous(labels = comma) + theme(axis.line = element_line(linetype = "solid"), 
                                             panel.background = element_rect(fill = NA), 
                                             strip.text = element_text(colour = NA), 
                                             legend.key = element_rect(fill = NA), 
                                             legend.background = element_rect(fill = NA))  
