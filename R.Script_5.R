rm(list=ls())
dev.off()

#rbinom(3, 10, c(0.1, 0.5, 0.9))
x = seq(from=0, to=1, by=0.01)
v_b = x*(1-x) #Binomial variance

plot(x, v_b, type="l", xlab="Probability", ylab="Theoretical variance", las=1)
# type=l gör attt kurvan blir en linje. p blir punkter, h blir histogram mm.

#Logit transformation####

logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x))
x = runif(200)
logit_x = logit(x)


par(mfrow=c(2,2)) #Delar plot ruta i 2 rader, 2 kolumner
hist(x, las=1)
hist(logit_x, las=1)
xx = seq(-5, 5, 0.01)
plot(xx, invlogit(xx), type="l", las=1,
     xlab="Logit (x)",
     ylab="P")

plot(x, invlogit(logit_x), las=1)
invlogit(logit(x)) #vilket är samma som x

#Problink, quantile distribution of the standard normal distribution#### 
par(mfrow=c(1,1))
plot(xx, invlogit(xx), type="l", las=1,
     xlab="Logit/Probit (x)", #Logit OR probit
     ylab="P")
lines(xx, pnorm(xx), lty=2)
legend("topleft", legend=c("Logit", "Probit"), #position, text, line type 1 and then 2
       lty=c(1,2))

#Skewed count data####

rm(list=ls())
x = rpois(200, 3)
hist(x, las=1)

#Logistic regression####

x = rnorm(200, 10, 3)
eta = -2 + 0.4*x + rnorm(200, 0, 2) #formulating a linear predictor η
invlogit = function(x) 1/(1+exp(-x)) 
p = invlogit(eta)  #transforming the predicted values
          #into probabilities (through the inverse logit transformation)
y = rbinom(200, 1, p)#binarizing the data by sampling from the binomial distribution.

par(mfrow=c(1,3))

plot(x, eta, las=1)
plot(x, p, las=1)
plot(x, y, las=1)

m = glm(y~x, family=binomial(link="logit"))
summary(m)

coefs = summary(m)$coef
x_pred = seq(from=min(x), to=max(x), by=0.01)
y_hat = coefs[1,1] + coefs[2,1]*x_pred #predictred y = intercept + slope*x_pred
p_hat = invlogit(y_hat) #Transformation to get probability scale


#log odds of 0 corresponds to a probability of 0.5 (log( 0.5/1−0.5 ) =
  #log(1) = 0). If we solve the model equation (the linear predictor) for 0, 
#we can thus obtain the predictor value corresponding to a probability of 0.5,
#0 = β0 + βx (intercept + slope)
# −β0/βx= x (-intercept/slope to get x when probability is 0.5)


-coefs[1,1]/coefs[2,1] #[1] 4.182042
par(mfrow=c(1,1))

plot(x, y, las=1) #Graf med 1 eller 0 punktvärden
lines(x_pred, p_hat, lty=1) #Lägg till sigmoid kurva mellan x_pred och p
abline( h=0.5, lty=2) #lägg till horisontell linje för y=0.5
abline(v=4.182042, lty=2) #lägg till vertikal linje för x=4.182042

library(MuMIn)


