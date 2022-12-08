rm(list=ls())

setwd("~/Documents/Documents/R/Statcourse/BIOS14")

plants = read.csv("alpineplants.csv")

plants = na.omit(plants)
plants = as.data.frame(scale(plants)) #z-transform
round(colMeans(plants), 2) #Check transformation
round(apply(plants, 2, sd), 2)

#Standard multipleregression
m1 = lm(Carex.bigelowii ~ snow + min_T_winter + soil_moist, data=plants)

#Fit several models seperately for later causal inference
m2a = lm(min_T_winter ~ snow, data=plants)
m2b = lm(soil_moist ~ snow, data=plants)
m2c = lm(Carex.bigelowii ~ min_T_winter + soil_moist, data=plants)

summary(m1)
#Issues with multicollinearity

#We can calculate the unexplained variance (“U”) in the response as √(1 −r2) 
#(which places it on the standardized [correlation] scale like the path coefficients).

#In this model we can calculate the total (net) effect of snow cover on the 
#abundance of Carex bigelowii by summing the direct effect and the effects 
#arising through correlations with other variables.

summary(m1)$coef[2,1] +
  summary(m1)$coef[3,1]*cor(plants$snow, plants$min_T_winter, "pairwise") +
  summary(m1)$coef[4,1]*cor(plants$snow, plants$soil_moist, "pairwise")
cor(plants$snow, plants$Carex.bigelowii, "pairwise") #Same as sum of above

summary(m2a)$coef
summary(m2b)$coef
summary(m2c)$coef

#Same on other data####

#Standard multipleregression
m1 = lm(Carex.bigelowii ~ altitude + mean_T_summer + soil_moist, data=plants)

#Fit several models seperately for later causal inference
m2a = lm(mean_T_summer ~ altitude, data=plants)
m2b = lm(soil_moist ~ altitude, data=plants)
m2c = lm(Carex.bigelowii ~ mean_T_summer + soil_moist, data=plants)

summary(m1)
#Issues with multicollinearity

cor(plants$altitude, plants$Carex.bigelowii, "pairwise")
#[1] 0.2882955

summary(m2a)$coef
summary(m2b)$coef
summary(m2c)$coef




