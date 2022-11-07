setwd("~/Documents/Documents/R/Statcourse/Exersice 2")

#Basic regression####

set.seed(85)
x = rnorm(n=200, mean=10, sd=2)
y = 0.4*x + rnorm(200, 0, 1)
plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")

m = lm(y~x)

cf = summary(m)$coef
View(cf)
predvals = cf[1,1] + cf[2,1]*x
par(mfrow=c(1,2)) # Delar plot fönster i 1 rad, 2 kolumner
plot(x, y, las=1)
abline(m)

segments(x, y, x, predvals)
hist(residuals(m), las=1)

par(mfrow=c(2,2)) #Delar plot fönster i 2 rader och 2 kolumner
plot(m) #Plotar fyra grafer om fördelning av data
summary(m) # Kollar quantiles för normalfördelning runt 0 (symmetrisk), 
          # x visar hur många enheter man ändrar i x led när man hoppar en enhet i y led
          # krävs 2 SE för att det ska vara signifikant

par(mfrow=c(1,1))
plot(x, y, las=1)

data <- data.frame(x,y) #create dataframe to keep paired samples
head(data)
sample(data[1,]) #sampling 1 row


V <- data[sample(nrow(data), replace=T),] #new vector sampling rows from data
sample(nrow(data), replace=T) #looking at row sampling in V

m1 = lm(y~x, data=V) #fitting model
vf = summary(m1)$coef # extracting coefficients
vf #viewing coefficients
vf[2,1] #extracting slope from coefficients


out=NULL
for (i in 1:1000) {
  V <- data[sample(nrow(data), replace=T),]
  sample(nrow(data), replace=T)
  
  m1 = lm(y~x, data=V)
  vf = summary(m1)$coef
  vf[2,1]
  out[i]=vf[2,1]
} #doing the V row sampling 1000 times in a loop

plot(out)
hist(out)

SE <- sqrt(var(out)/length(out))
SE

cov(y,x)/var(x)
coefs = summary(m)$coef
(coefs[2,1]*(mean(x) + sd(x))) - (coefs[2,1]*mean(x)) #calculate change in y for 1 change in SDx
cor(x,y)^2


y_hat = coefs[1,1] + coefs[2,1]*x
var(y_hat)
var(y_hat)/var(y)

coefs[2,1]^2*var(x)

newx = seq(min(x), max(x), length.out=200)
predy = coefs[1,1] + coefs[2,1]*newx #genetates new predicted y values

plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")
lines(newx, predy)


#Errors in x####

rm(list=ls())


x = rnorm(500, 10, 2)
y = 1.5*x + rnorm(500, 0, 1)
slope_est = NULL
errors = seq(0.01, 0.5, length.out=10)
errors
relerrors = (errors^2)/var(x)

for(i in 1:10){
  x_obs = x + rnorm(500, 0, errors[i])
  m1 = lm(y~x_obs)
  slope_est[i] = summary(m1)$coef[2,1]
}


plot(errors, slope_est,
     las=1,
     xlab="Error standard deviation in x",
     ylab="Estimated slope")

relerrors
corrslope = slope_est/(1-relerrors)

plot(errors, slope_est,
     ylim= c(1.4, 1.55),
     las=1,
     xlab="Error standard deviation in x",
     ylab="Estimated slope")
points(errors, corrslope, pch=16)
segments(errors, slope_est, errors, corrslope)

#Errors in y####
rm(list=ls())


x = rnorm(500, 10, 2)
y = 1.5*x + rnorm(500, 0, 1)
slope_est = NULL
errors = seq(0.01, 0.5, length.out=10)
errors
relerrors = (errors^2)/var(y)

for(i in 1:10){
  y_obs = y + rnorm(500, 0, errors[i])
  m1 = lm(x~y_obs)
  slope_est[i] = summary(m1)$coef[2,1]
}


plot(errors, slope_est,
     las=1,
     xlab="Standard deviation in x",
     ylab="Error estimated slope")

relerrors
corrslope = slope_est/(1-relerrors)

plot(errors, slope_est,
     ylim= c(0.59, 0.64),
     las=1,
     xlab="Standard deviation in x",
     ylab="Error estimated slope")
points(errors, corrslope, pch=16)
segments(errors, slope_est, errors, corrslope)

#Model real data####

rm(list=ls())

birds = read.csv("bird_allometry.csv", sep=',')
head(birds)

hist(birds$brain_mass)
hist(birds$body_mass)
qqnorm(birds$body_mass)
qqnorm(birds$brain_mass)

logbrain <- log(birds$brain_mass)
birds$logbrain <- logbrain

hist(birds$logbrain)
qqnorm(birds$logbrain)

logmass <- log(birds$body_mass)
birds$logmass <- logmass

hist(birds$logmass)
qqnorm(birds$logmass)

m = lm(birds$logmass~birds$logbrain)

plot(birds$logmass~birds$logbrain,
  las=1,
  xlab="Log body mass (g)",
  ylab="Log brain mass (g)")
abline(m)
