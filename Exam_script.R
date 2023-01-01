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

dat = na.omit(dat) #remove missing values

#Visualizing data####

hist(dat$GSD) #Normal distribution
hist(dat$UBW) #Normal distribution
hist(dat$LBW) #Normal distribution
hist(dat$UBL) #Normal distribution
hist(dat$LBL) #Normal distribution

plot(UBW ~ LBW, data=dat) #Basic plot of blossom size 

m = lm(UBW ~ LBW, data=dat) #Check that lower and upper bract width correlate
summary(m) #Strong correlation, R=0.91


#Plotting correlation

plot(UBW ~ LBW, data=dat, las=1,
     xlab = "Lower bract width (mm)",
     ylab = "Upper bract width (mm)")
abline(m)

#Question: Does wet/dry treatment affect blossom size and does the effect differ
#between the two species?

#Normally distributed data so parametric test are allowed

#I will use UBW as a measurement for entire blossom size since there was a 
#strong correlation between LBW and UBW.

m2 = lm(UBW ~ treat, data=dat)
summary(m2) #Big difference in blossom size, wet is bigger

#Take non independent measurements from the same table and populations into account

library(glmmTMB)

m3 = glmmTMB(UBW ~ treat + (1|table) + (1|pop), dat=dat)
summary(m3)  #Still big difference where wet is bigger
#More variance explained by population than table

#Plotting overall treatment differences####

#Create statistics for error bars
library(plyr)

stats <- ddply(dat,"treat", summarize, N=length(UBW),
                     mean_UBW=mean(na.omit(UBW)), #calculate mean
                     sd=sd(na.omit(UBW)), #calculate standard deviation
                     se=sd/sqrt(N)) #calculate standard error

#Change name of treatments

stats$treat = gsub("D", "Dry", stats$treat)
stats$treat = gsub("W", "Wet", stats$treat)

#Creating barplot with errorbars

library(ggplot2)

ggplot(stats, aes(x=treat, y=mean_UBW, fill = treat)) + #set plotting variables
  geom_bar(width = 0.75, stat = "identity", position ="dodge", alpha = 0.8) + #determine plot type
  geom_errorbar(data=stats, aes(ymin = mean_UBW - se, 
                              ymax = mean_UBW + se),
                width = 0.13) + #specify error bars as mean +/- se
  theme_classic() + #theme of plot
  scale_fill_grey() + #set fill color to grey
  scale_y_continuous(limits = c(0,50), expand = c(0,0)) + #set scale for y axis
  labs(y="Mean upper bract width (mm)", x="", 
       title = "") + #specify axis labels and title label
  theme(legend.position = c(0.9,0.9), 
        legend.title = element_blank(),
        plot.title = element_text (hjust = 0.5),
        text = element_text(size=28, family= "Times"),
        axis.text.x = element_text(size = 20, angle = 0,
                                   hjust = 0.5, color = "black")) + #set size, color, position and theme of text in plot
  theme(axis.ticks.length=unit(1, "mm")) #set length for ticks on the axes

ggsave("blossom_size.png", plot = last_plot(), device = "png",
       scale = 1, width = 13, height = 8,
       dpi = 600) #saving plot at fixed size

#Does the effect of treatment differ between species?####

m4 = lm(UBW ~ sp*treat, data=dat)
anova(m4) #Effect of both species and treatment on UBW, treatment larger effect
#taken from bigger sum.sq, small but detectable interaction
summary(m4)

#Computing means and standard error for species in each treatment

means = tapply(dat$UBW, list(dat$sp, dat$treat), mean)

se = tapply(dat$UBW, 
             list(dat$sp, dat$treat), 
             function(x) sd(x)/sqrt(sum(!is.na(x))))

#Plotting difference between treatments in each species####

#Set blank plot environment with x and y limits and labels
plot(c(0.97, 1.03), means[,1], ylim=c(10, 30), xlim=c(0.8, 2.2),
     xlab="Treatment", 
     ylab="Upper bract width (mm)",
     xaxt="n", las=1, pch=c(21,16), col="white") 

#Create label for each treatment on x axis
axis(1, 1:2, labels=c("Dry", "Wet")) 

#Add +/- se error bars for each species for the dry treatment (means[,1]),
#length sets error bar width, angle sets rotation, c() sets position on x, 
#code 3 sete type of error bar to one with both upper and lower whiskers 
arrows(c(0.97,1.03), means[,1] - se[,1], c(0.97,1.03), 
       means[,1] +se [,1], length=0.05, angle=90, code=3)
#Add +/- se error bars to the wet treatment (means[,2])
arrows(c(1.97,2.03), means[,2] - se[,2], c(1.97,2.03), 
       means[,2] + se[,2], length=0.05, angle=90, code=3)

#Add line between mean dry and wet for L species and then S species
segments(0.97, means[1,1], 1.97, means[1,2], lty=2)
segments(1.03, means[2,1], 2.03, means[2,2])

#Add points on positions c() to first dry then wet treatment, pch sets aesthetics for points,
#bg determines color of blank points
points(c(0.97, 1.03), means[,1], pch=c(21,16), bg="white")
points(c(1.97, 2.03), means[,2], pch=c(21,16), bg="white")

#Add legend to topleft
legend("topleft", c("Species", "L", "S"), 
       bty="n", pch=c(NA,21,16))


