rm(list = ls()) #Clear environment
list.files ()

setwd("~/Documents/Documents/R/Statcourse/BIOS14") #Setting working directory

################################PART 1#######################################

dat = read.csv("exam2022_part1.csv") #Read data file

#Data sorting####

str(dat)
head(dat)

#Setting treatment, pop, species and plant as factors
dat$treat = as.factor(dat$treat)
dat$pop = as.factor(dat$pop)
dat$sp = as.factor(dat$sp)
dat$plant = as.factor(dat$plant)

dat = na.omit(dat) #remove missing values

#Visualizing data####

hist(dat$GSD) #Normal distribution
hist(dat$UBW) #Normal distribution
hist(dat$LBW) #Normal distribution
hist(dat$UBL) #Normal distribution
hist(dat$LBL) #Normal distribution
hist(dat$GA) #Normal distribution

plot(UBW ~ LBW, data=dat) #Basic plot of blossom size 

#Fit model
m = lm(UBW ~ LBW, data=dat) #Check that lower and upper bract width correlate
summary(m) #Strong correlation, R=0.91


#Plotting correlation

plot(UBW ~ LBW, data=dat, las=1,
     xlab = "Lower bract width (mm)",
     ylab = "Upper bract width (mm)")
abline(m) #Add regression line from model m

#Question: Does wet/dry treatment affect blossom size and does the effect differ
#between the two species?

#Normally distributed data so parametric models are used

#I will use UBW as a measurement for entire blossom size since there was a 
#strong correlation between LBW and UBW.

m2 = lm(UBW ~ treat, data=dat)
anova(m2)
summary(m2) #Big difference in blossom size, wet is bigger

#Check variance among and within tables

library(glmmTMB)

m3 = glmmTMB(UBW ~ 1 + (1|table), dat=dat)
summary(m3)  

VarCorr(m3)
VarAmongTable = attr(VarCorr(m3)$cond$table,"stddev")^2
#0.117
VarWithinTable = attr(VarCorr(m3)$cond,"sc")^2
#14.1577804898716
#Total variance explained by table
VarAmongTable/(VarAmongTable+VarWithinTable)*100
#0.8193984  Less than 1% variance explained

#Check variance among and within populations
m4 = glmmTMB(UBW ~ 1 + (1|pop), dat=dat)
summary(m4)  

VarCorr(m4)
VarAmongPop = attr(VarCorr(m4)$cond$pop,"stddev")^2
#0.653
VarWithinPop = attr(VarCorr(m4)$cond,"sc")^2
#13.7134380263794
#Total variance explained by pop
VarAmongPop/(VarAmongPop+VarWithinPop)*100
#4.5451   Still not a lot of variance explained

#Check variance among and within plants
m5 = glmmTMB(UBW ~ 1 + (1|plant), dat=dat)
summary(m5)  

VarCorr(m5)
VarAmongPlant = attr(VarCorr(m5)$cond$plant,"stddev")^2
#0.804
VarWithinPlant = attr(VarCorr(m5)$cond,"sc")^2
#13.4697013860856
#Total variance explained by plant
VarAmongPlant/(VarAmongPlant+VarWithinPlant)*100
#5.63273 Still not a lot of variance explained, variance explained by fixed predictor

#Take variance from table and plant into account

m6 = glmmTMB(UBW ~ treat + (1|table) + (1|plant), dat=dat)
summary(m6) #Still big difference where wet is bigger

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

#Creating barplot with error bars

library(ggplot2)

ggplot(stats, aes(x=treat, y=mean_UBW, fill = treat)) +                         #set plotting variables
  geom_bar(width = 0.75, stat = "identity", position ="dodge", alpha = 0.8) +   #determine plot type
  geom_errorbar(data=stats, aes(ymin = mean_UBW - se, 
                              ymax = mean_UBW + se),
                width = 0.13) +                                                 #specify error bars as mean +/- se and set error bar width
  theme_classic() +                                                             #theme of plot
  scale_fill_grey() +                                                           #set fill color to grey scale
  scale_y_continuous(limits = c(0,50), expand = c(0,0)) +                       #set length for y axis and expand 0
  labs(y="Mean upper bract width (mm)", x="", 
       title = "") +                                                            #specify text on axis labels and title, blank for title and x axis
  theme(legend.position = c(0.9,0.9),                                           #set position of legend
        legend.title = element_blank(),                                         #remove legend title
        text = element_text(size=28, family= "Times"),                          #set size and font for text in plot
        axis.text.x = element_text(size = 20, angle = 0,
                                   hjust = 0.5, color = "black")) +             #set size, angle, position and color for text on x axis
  theme(axis.ticks.length=unit(0.25, "cm"))                                     #set length for ticks on the axes

#saving plot as png at fixed size

ggsave("blossom_size.png", plot = last_plot(), device = "png",
       scale = 1, width = 13, height = 8,
       dpi = 600) 

#Does the effect of treatment differ between species?####

m7 = lm(UBW ~ treat*sp, data=dat)
anova(m7) #Effect of both species and treatment on UBW, treatment larger effect
#taken from bigger sum.sq, small but detectable interaction
summary(m7)

#Suppress intercept
m7 = lm(UBW ~ treat*sp -1, data=dat)
anova(m7) #Effect of both species and treatment, effect of interaction as well
#but not a lot of variance explained, most variance explained by treatment
summary (m7)

#Computing means and standard error for species in each treatment

means = tapply(dat$UBW, list(dat$sp, dat$treat), mean)

se = tapply(dat$UBW, 
             list(dat$sp, dat$treat), 
             function(x) sd(x)/sqrt(sum(!is.na(x))))
colMeans(means)
#       D        W 
#16.82407 21.67662 

rowMeans(means)
#       L        S 
#20.14935 18.35134 

colMeans(se)
#D         W 
#0.3010410 0.2740316 

rowMeans(se)
#    L         S 
#0.2928040 0.2822687 

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

################################PART 2#######################################

rm(list=ls())  #empty environment
list.files ()

dat = read.table("exam2022_part2.txt", header=T) #read new data file

#Data Handling####

str(dat)
head(dat)

dat$sex = as.factor(dat$sex) #set sex as a factor

hist(dat$mass) #Visualizing data distribution for mass -> normal distribution

plot(mass ~ sex, data=dat) #basic plot showing data distribution of mass between sexes

#Does the mass of goats differ between sexes?

#Fit model####

m = lm(mass ~ sex, data=dat)
anova(m) #Difference between sexes
summary(m) #Males bigger than females

#Suppress intercept

m1 = lm(mass ~ sex-1, data=dat)
summary(m1)
confint(m1)

#Calculating means, sd and se for mass#####

library(plyr)

stats = ddply(dat, "sex", summarize, N=length(mass),
              mean = mean(mass),
              sd = sd(mass),
              se = sd/sqrt(length(mass))) 

#Plotting#####

#Change name of sex variable for plotting

stats$sex = gsub("F", "Female", stats$sex)
stats$sex = gsub("M", "Male", stats$sex)

library(ggplot2)

ggplot(stats, aes(x=sex, y=mean, fill = sex)) +                                 #Set plotting parameters
  geom_bar(width = 0.75, stat = "identity", position ="dodge", alpha = 0.8) +   #Set type of plot and positioning of bars
  geom_errorbar(data=stats, aes(ymin= mean - se, ymax=mean + se),               #Set error bars
                width = 0.13, alpha = 1, position=position_dodge(0.75)) +       #Set dimensions of error bars
  theme_classic() +                                                             #Set theme of plot
  scale_fill_grey() +                                                           #Set color of bars to grey scale
  scale_y_continuous(limits = c(0,50), expand = c(0,0)) +                       #Set y axis limit and expand the 0
  labs(y="Mean mass (kg)", x="", 
       title = "") +                                                            #Specify text on axes and title, blank for x and title
  theme(legend.position = c(0.9,0.9),                                           #Set legend position
        legend.title = element_blank(),                                         #Remove legend title
        text = element_text(size=28, family= "Times"),                          #Set size and font for text in plot
        axis.text.x = element_text(size = 20, angle = 0,
                                   hjust = 0.5, color = "black")) +             #Set size, angle, position and color of text on x axis
  theme(axis.ticks.length=unit(.25, "cm"))                                      #Lengthen the ticks on the axes in the graph

#Save plot as PNG image and set size of image

ggsave("Mass_barplot.png", plot = last_plot(), device = "png",
       scale = 1, width = 12, height = 8,
       dpi = 600)
