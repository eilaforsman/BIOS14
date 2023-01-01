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





