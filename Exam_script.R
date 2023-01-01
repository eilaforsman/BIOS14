rm(list = ls()) #Clear environment
list.files ()

setwd("~/Documents/Documents/R/Statcourse/BIOS14")

dat = read.csv("exam2022_part1.csv") #Read datafile

#Data sorting####

str(dat)
head(dat)

dat$treat = as.factor(dat$treat)
dat$pop = as.factor(dat$pop)
dat$sp = as.factor(dat$sp)

#Visualizing data####

hist(dat$GSD) #Normal distribution
hist(dat$UBW) #Normal distribution
hist(dat$LBW) #Normal distribution
hist(dat$UBL) #Normal distribution
hist(dat$LBL) #Normal distribution

plot(UBW ~ LBW, data=dat)

m = lm(UBW ~ LBW, data=dat) #Check that lower and upper bract width correlate
summary(m) #Strong correlation, R=0.91


#Plotting correlation

plot(UBW ~ LBW, data=dat, las=1,
     xlab = "Lower bract width (mm)",
     ylab = "Upper bract width (mm)")
abline(m)

#Question: Does wet/dry treatment affect blossom size?

#Normally distribted data so parametric test are allowed

#I will use UBW as a measurement for entire blossom size since there was a 
#strong correlation between LBW and UBW.

m2 = lm(UBW ~ treat, data=dat)
summary(m2) #Big difference in blossom size, wet is bigger

#Take non independent measurements from the same table into account

library(glmmTMB)

m3 = glmmTMB(UBW ~ treat + (1|table), dat=dat)
summary(m3)  #Still big difference where wet is bigger

#Take possible different responses between species and populations into account 
m4 = glmmTMB(UBW ~ treat + (1|table) + (1|pop/sp), dat=dat)
summary(m4) 

#The wet treatment led to bigger blossoms

#Plotting treatment differences####

#Create statistics for errorbars
library(plyr)

stats <- ddply(dat,"treat", summarize, N=length(UBW),
                     mean_UBW=mean(na.omit(UBW)),
                     sd=sd(na.omit(UBW)),
                     se=sd/sqrt(N))

#Change name of treatments

stats$treat = gsub("D", "Dry", stats$treat)
stats$treat = gsub("W", "Wet", stats$treat)

#Creating barplot with errorbars

library(ggplot2)

ggplot(stats, aes(x=treat, y=mean_UBW, fill = treat)) +
  geom_bar(width = 0.75, stat = "identity", position ="dodge", alpha = 0.8) +
  geom_errorbar(data=stats, aes(ymin = mean_UBW - se, 
                              ymax = mean_UBW + se),
                width = 0.13) +
  theme_classic() + 
  scale_fill_grey() +
  scale_y_continuous(limits = c(0,50), expand = c(0,0)) +
  labs(y="Mean upper bract width (mm)", x="", 
       title = "") +
  theme(legend.position = c(0.9,0.9), 
        legend.title = element_blank(),
        plot.title = element_text (hjust = 0.5),
        text = element_text(size=28, family= "Times"),
        axis.text.x = element_text(size = 20, angle = 0,
                                   hjust = 0.5, color = "black")) +
  theme(axis.ticks.length=unit(.25, "mm"))

ggsave("blossom_size.png", plot = last_plot(), device = "png",
       scale = 1, width = 13, height = 8,
       dpi = 600)






