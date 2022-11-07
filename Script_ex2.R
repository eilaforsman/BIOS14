setwd("~/Documents/Documents/R/Statcourse/Exersice 2")

#Section 1####

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
