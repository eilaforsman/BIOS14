#GLM-binomial and poission#####

rm(list=ls())

#A second very common data type in biology is count data, which occurs when we 
#have counted something. For such data, the data distribution is often skewed, 
#and the variance tends to increase with the mean. The Poisson distribution is 
#tailored for such data.

x = rpois(200, 3) #200 values, mean 3
hist(x, las=1)


#The Poisson distribution has a single parameter λ that determines both the mean 
#and the variance, so that E(X) = V ar(X) = λ. Thus, the variance increases linearly 
#with the mean.

x = seq(0, 20, 1) #From 0 to 20, interval 1
y = dpois(x, lambda=1)
plot(x, y, type="b", las=1, xlab="k", ylab="P(x=k)", pch=16, col=1)
points(x, dpois(x, lambda=3), type="b", pch=16, col=2) #Add b type line with lambda=3
points(x, dpois(x, lambda=10), type="b", pch=16, col=3) #Add b type line with lambda=10
legend("topright", col=1:3, pch=16,
       legend=c(expression(paste(lambda, " = 1")),
                expression(paste(lambda, " = 3")),
                expression(paste(lambda, " = 10")))) #Add legend with colors 1-3 and text

#The alternative method of log-transforming the data and then fitting a Gaussian model 
#is problematic when there are zeros in the data. Adding a constant
#(e.g. 0.5 or 1) is sometimes an option, but is generally not recommended. 
#A better option is to analyze the data in a GLM framework with Poisson-distributed errors 
#and a log link function.

x = rnorm(200, 10, 3)
eta = -2 + 0.2*x
y = ceiling(exp(eta + rpois(200, 0.3)))

par(mfrow=c(1,2)) #Split plot environment
plot(x, eta, las=1)
plot(x, y, las=1)


m = glm(y~x, family="poisson") #Note residual deviance to check for overdispersion 
#(residual deviance/df) 0.7-1.3 is good
summary(m)

#As in all GLMs, the parameters are reported on the link scale, here log. 
#Recall that the link scale has useful proportional properties, so that the slope 
#can be interpreted roughly as the proportional change in y per unit change in x. 


#Om man ökar x en enhet ändras log(y) 0.128 =12.8% (x estimate), e^0.128 ger 
#hur mycket y ändras om man ökar x en enhet


#To plot the fitted regression line, we have to back-transform the predicted values.
plot(x, y, las=1, col="darkgrey", pch=16)
xx = seq(min(x), max(x), 0.01)
y_hat = predict(m, newdata=list(x=xx), type="response", se.fit=T) 
#predict(model, newdata=list(), type=response does back transformation automatically, 
#link presents link scale)
lines(xx, y_hat$fit)

#lines(xx, y_hat$fit+1.96*y_hat$se.fit, lty=2)  (Adds dotted line for + 1SE)
#lines(xx, y_hat$fit-1.96*y_hat$se.fit, lty=2)  (Adds dotted line for - 1SE)

polygon(c(xx, rev(xx)),
        c(y_hat$fit+1.96*y_hat$se.fit,
          rev(y_hat$fit-1.96*y_hat$se.fit)),
        col = rgb(0,1,0,.5), border = FALSE)

#As in logistic regression the normal r2 is not valid, and we can use e.g. 
#the r.squaredGLMM function to obtain a Pseudo r2. For a simple GLM an older 
#Pseudo r2 is 1 − Residual deviance/ Null deviance

1-(m$deviance/m$null.deviance)

#In real count data the variance often increase disproportionally compared to 
#the mean, a phenomenon called overdispersion. Biologically, this occurs because 
#we tend to observe multiple entities together. For example, in a survey of common 
#eiders, we will often see either no eiders, or a lot of eiders.

#We can quantify overdispersion in the data based on the fitted model by 
#calculating the ratio of the residual deviance to the residual degrees of freedom.

#Let us construct an example with more serious overdispersion.

rm(list=ls())

set.seed(1)
x = rnorm(200, 10, 3)
eta = -2 + 0.2*x
y = floor(exp(eta + rnbinom(200, 1, mu=.8)))

par(mfrow=c(1,2))
plot(x, eta, las=1)
plot(x, y, las=1)

m = glm(y~x, family="poisson")
summary(m)

#Here the overdispersion is serious, and we can not trust the model estimates. 
#In this case, we need an alternative link function that allows the variance to 
#increase more than the mean. The negative binomial distribution is a good option.
#The negative binomial is similar to the Poisson distribution, but includes an
#additional parameter modelling the disproportionate increase in variance with 
#increasing mean.

library(MASS)
m = glm.nb(y~x)
summary(m) #Note change in residual deviance and higher SE

#Data exersice 6####

rm(list=ls())

dat = read.csv("Eulaema.csv")
head(dat)

hist(dat$altitude)
m = glm.nb (dat$Eulaema_nigrita~dat$Pseason)
summary(m)

hist(dat$MAT)
m1 = glm.nb (dat$Eulaema_nigrita~dat$MAT)
summary(m1)

hist(dat$MAP)
m2 = glm.nb(dat$Eulaema_nigrita~dat$MAP)
summary(m2)

hist(dat$lu_het)
m3 = glm.nb (dat$Eulaema_nigrita~dat$lu_het)
summary(m3)

hist(dat$forest.)
m4 = glm.nb(dat$Eulaema_nigrita~dat$forest.)
summary(m4)


m5 = glm.nb (dat$Eulaema_nigrita ~ dat$Pseason + dat$lu_het)
summary(m5)

#Medelantal bin är e^2.4428 = 11,5, om land use ändras en enhet ökar bin e^0.6=1.8, 
#om Pseason ändras ökar bin med e^0.025=1.

x1_m = (dat$Pseason - mean(dat$Pseason))/mean(dat$Pseason)
x2_m = (dat$lu_het - mean(dat$lu_het))/mean(dat$lu_het)
summary(lm(dat$Eulaema_nigrita ~ x1_m + x2_m))

m5 = glm.nb (dat$Eulaema_nigrita ~ x1_m + x2_m)
summary(m5) #Comparable effect of predictors, Pseason higher impact than lu_het OBS still log

plot(dat$Eulaema_nigrita ~ dat$Pseason + dat$lu_het) #KAOS

#Hurdle models####

rm(list=ls())

#n biology we often have count data with many zeros. This is one of the causes 
#of overdispersion as discussed above. Another way to deal with this is to split 
#the analysis into two independent components, where the first models the zeros, 
#and the second models the counts given that at least 1 entity was observed. This is
#called a hurdle model, where hurdle refers to the separation of zeros from non-zeros.
#We can fit a hurdle model in two parts and then combine the predictions.

set.seed(1)
x = rnorm(200, 10, 3)
eta = -2 + 0.2*x
y = floor(exp(eta + rnbinom(200, 1, mu=.8)))


y1 = ((y>1)*1)
m1 = glm(y1~x, family="binomial" (link="logit"))
summary(m1)

y2 = y
y2[which(y==0)] = NA
m2 = glm(y2~x, family="poisson", na=na.exclude)
summary(m2)

coefs1 = summary(m1)$coef
coefs2 = summary(m2)$coef
y_hat1 = coefs1[1,1] + coefs1[2,1]*x
y_hat2 = coefs2[1,1] + coefs2[2,1]*x


invlogit = function(x) 1/(1+exp(-x))
y_pred = invlogit(y_hat1)*exp(y_hat2)

par(mfrow=c(1,3))
plot(x, invlogit(y_hat1), las=1)
plot(x, exp(y_hat2), las=1)
plot(x, y_pred, las=1)
