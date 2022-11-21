#Exersice 3####

setwd("~/Documents/Documents/R/Statcourse/BIOS14")

rm(list=ls())

set.seed(100)
groups = as.factor(rep(c("Small", "Medium", "Large"), each=50))
x = c(rnorm(50, 10, 3), rnorm(50, 13, 3), rnorm(50, 14, 3)) #skapar grupper med 50 värden, mean, sd

plot(groups, x, las=1, xlab="")

{r, fig.width=4, fig.height=4, echo=F} #Fancy plot som visar data fördelning brevid
plot(as.numeric(groups) + rnorm(150,0,0.03), x, #Plottar bara data
     las=1, xlab="", type="p", col="grey",
     xlim=c(0.5, 3.75), xaxt="n", yaxt="n") 
axis(1, 1:3, labels=levels(groups)) #Lägger till gruppnamn
means = tapply(x, groups, mean) #räknar ut medel för alla grupper
points(1:3, means, pch=16, col="black") #ritar medel som punkter
par(new=T) #Ny "plot" inuti förra ploten
plot(groups, x, at=c(1.3, 2.3, 3.3), boxwex=0.3, #Lägger till vanlig boxpot med bredd 0.3
     xlim=c(0.5, 3.75), xaxt="n", yaxt="n",
     las=1, xlab="", ylab="")

m = lm(x~groups) #fit model med grupper
anova(m) #Utför ANOVA, titta speciellt på Sum Sq. The total sum of squared (SST ) 
#divided by n −1 gives the total variance of the sample.

#For the groups variable (our focal factor), the 2 degrees of freedom is the number
#of groups in our data (3) - 1. The minus 1 comes from the fact that we had to estimate
#the mean in the data to obtain our sums of squares 
#(the sum of the square deviations of data points from their group means). Similarly
#for residual degrees of freedom, we have 150 - 2 - 1, where the 2 comes from 
#estimating the two contrasts
#(difference of group 2 and 3 from group 1), and the 1 is still the estimated mean.

SS_T = 319.97+1200.43
SS_T/(150-1)
var(x)
#We can easily get the proportion of variance explained by the groups variable, 
#which is the same as the r2
319.97/SS_T

summary(m)

3.4561/13.7006 # divide sample by reference group to get % difference between groups

groups = factor(groups, levels=c("Small", "Medium", "Large")) #Set order
m = lm(x~groups) #fit model
summary(m) #Run summary again this time small is the reference group

m = lm(x~groups-1) #Remove intercept to only get mean for each group an 95%CI
summary(m)$coef
confint(m) #95%CI

#Two-way ANOVA#####

rm(list=ls())

dat = read.csv("butterflies.csv")
names(dat)
head(dat)

m = lm(dat$AdultWeight ~ dat$LarvalHost*dat$MaternalHost)
anova(m)
