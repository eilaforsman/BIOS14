#Multiple regression####

rm(list=ls())

set.seed(187)
x1 = rnorm(200, 10, 2)
x2 = 0.5*x1 + rnorm(200, 0, 4)
y = 0.7*x1 + 2.2*x2 + rnorm(200, 0, 4)
m = lm(y~x1+x2)
coefs = summary(m)$coef
summary(m) #Intercept is value of y when both x's are 0, estimates are the effet of each variable.
            #need to standardize to compare effects (eg. with sd)
y_hat = coefs[1,1] + coefs[2,1]*x1 + coefs[3,1]*x2 #computing the variance in the
                                                #predicted values ˆy, V (ˆy) = V (Xβ)
var(y_hat)
var(y_hat)/var(y) #same as r^2
#Now what about the variance explained by each of the
#predictors x1 and x2? To compute the predicted values associated only with x1, 
#we keep x2 constant at its mean, and vice versa for the variance associated with x2

y_hat1 = coefs[1,1] + coefs[2,1]*x1 + coefs[3,1]*mean(x2)
var(y_hat1)
var(y_hat1)/var(y) #variance associated with x1

y_hat2 = coefs[1,1] + coefs[2,1]*mean(x1) + coefs[3,1]*x2
var(y_hat2)
var(y_hat2)/var(y) #varaiance associated with x2

var(y_hat)
var(y_hat1) + var(y_hat2) #So, what happened to the last few percent of the variance? 
#Recall that Var(x + y) = Var(x) + Var(y) + 2Cov(x,y).

var(y_hat1) + var(y_hat2) + 2*cov(y_hat1, y_hat2)# same as var(y_hat)

#As before, we can also do this by computing V (x) = (βofx)^2*(σofx)^2.
coefs[2,1]^2*var(x1)
#To include the covariance between the predictors, we can do this in matrix notation 
#V (ˆy) = ˆβTSˆβ, where
#ˆβ is a vector of parameter estimates (slopes), Sis the covariance matrix for the 
#"predictors, and T means transposition. Recall the R matrix multiplication operator %*%.

t(coefs[2:3,1]) %*% cov(cbind(x1,x2)) %*% coefs[2:3,1] 

#The most common way to standardize predictor variables is to
#scale them to zero mean and unit variance, a so-called z-transform
#z = x−̄mean(x)/sd(x)
#The resulting variable will have a mean of zero and a standard deviation
#(and variance) of one (remember to check that this is indeed the case).

x1_z = (x1 - mean(x1))/sd(x1)
x2_z = (x2 - mean(x2))/sd(x2)
m = lm(y ~ x1_z + x2_z)
summary(m) #now we can compare estimates to see which x has most effect. Note r^2 is same
            #now intercept is the mean value of y, slopes is in unit sd

#nother useful transformation could be a natural log-transform, or similarly 
#mean-scaling, which would give the slopes units of means, and allow interpreting 
#the change in y per percent change in x. These proportional
#slopes are technically called elasticities.

x1_m = (x1 - mean(x1))/mean(x1)
x2_m = (x2 - mean(x2))/mean(x2)
summary(lm(y ~ x1_m + x2_m)) #Intercept still y mean, slopes is % change

#Multicollinearity######


#When we have several predictors that are strongly correlated with each other, 
#it becomes difficult to estimate their independent effects. A rule of thumb is 
#that such multicollinearity becomes a potential problem when the correlation 
#between the predictors is greater than 0.6 or 0.7. One way of assessing the degree 
#of multicollinearity is to compute variance inflation factors, defined as
#VIFi = 1/1−r2i
#where the r2 is from a regression of covariate i on the other covariates included in the model. 

m1 = lm(x1~x2)
r2 = summary(m1)$r.squared
1/(1-r2)

#Rules of thumb for what constitutes
#severe variance inflation range from V IF > 3 to V IF > 10
#When this occurs, the parameter estimates become associated with excessive variance 
#and are thus less reliable. In these cases it may be good to
#simplify the model by removing some of the correlated predictors, especially if 
#there are several predictors
#that essentially represent the same property (e.g. multiple measures of body size).

#Data exercise: multiple regression and variable selection#####

rm(list=ls())

plants = read.csv("alpineplants.csv")
plants
View(plants)
summary(plants)
plot(data=plants, mean_T_winter~mean_T_summer)


plot(plants$Carex.bigelowii~plants$mean_T_summer)
plot(plants$Carex.bigelowii~plants$light)
plot(plants$Thalictrum.alpinum~plants$mean_T_summer)

m1 = lm(plants$Thalictrum.alpinum~plants$mean_T_summer)
summary(m1)
qqnorm(plants$mean_T_summer)
qqnorm(plants$Carex.bigelowii)
qqnorm(plants$Thalictrum.alpinum)
plants$logCarex = log(plants$Carex.bigelowii+1)

qqnorm(plants$logCarex)
plants$logThal = log(plants$Thalictrum.alpinum+1)
qqnorm(plants$logThal)

plants$sqrt.carex = sqrt(plants$Carex.bigelowii)

hist(plants$sqrt.carex)
hist(plants$Carex.bigelowii)

mr = lm(plants$Carex.bigelowii~plants$mean_T_winter+plants$mean_T_summer)
summary(mr)

qqnorm(plants$soil_moist)
plot(plants$Carex.bigelowii~plants$soil_moist)
plot(plants$Thalictrum.alpinum~plants$soil_moist)

plot(plants$soil_moist~plants$mean_T_summer)



sat = lm(plants$Carex.bigelowii~plants$mean_T_summer+plants$soil_moist+
           plants$mean_T_winter+plants$light+plants$altitude+plants$max_T_winter+
           plants$max_T_summer+plants$min_T_winter+plants$min_T_summer)
summary(sat)

m = lm(plants$Carex.bigelowii~plants$altitude+plants$mean_T_summer+plants$mean_T_winter)
summary(m)
coefs = summary(m)$coef

hist(residuals(m))

m1 = lm(plants$sqrt.carex~plants$altitude+plants$mean_T_summer+plants$mean_T_winter)
hist(residuals(m1)) #Regular was better
summary(m1)

plot(plants$Carex.bigelowii~plants$altitude)
plot(plants$Carex.bigelowii~plants$mean_T_winter)
plot(plants$Carex.bigelowii~plants$mean_T_summer)

m2 = lm(plants$mean_T_summer~plants$altitude)
r2 = summary(m2)$r.squared
1/(1-r2) #[1] 2.946908 

coefs=summary(m1)$coef
coefs
y_hat1 = coefs[1,1] + coefs[2,1]*plants$altitude + coefs[3,1]*mean(plants$mean_T_summer, na.rm=T) + 
  coefs[4,1]*mean(plants$mean_T_winter,na.rm = T)
var(y_hat1)
var(y_hat1)/var(plants$Carex.bigelowii)#[1] 0.03662726

y_hat2 = coefs[1,1] + coefs[2,1]*mean(plants$altitude, na.rm=T) + coefs[3,1]*plants$mean_T_summer + coefs[4,1]*mean(plants$mean_T_winter,na.rm = T) 
var(y_hat2, na.rm=T)/var(plants$Carex.bigelowii) #[1] 0.03473849


y_hat3 = coefs[1,1] + coefs[2,1]*mean(plants$altitude, na.rm=T) + coefs[3,1]*mean(plants$mean_T_summer, na.rm=T) + 
  coefs[4,1]*plants$mean_T_winter
var(y_hat3, na.rm=T)/var(plants$Carex.bigelowii) #[1] 0.03542211


na.omit(plants$mean_T_winter)


#ANCOVA####

#A statistically supported interaction means that the slopes differ between groups, 
#while a statistically supported main effect of groups means that the intercepts differ.
rm(list=ls())

set.seed(12)
x = rnorm(200, 50, 5)
gr = factor(c(rep("Male", 100), rep("Female", 100)))
y = -2 + 1.5*x + rnorm(200, 0, 5)
y[101:200] = 2 + 0.95*x[101:200] + rnorm(100, 0, 6)

plot(x, y, pch=c(1,16)[as.numeric(gr)], las=1)
