rm(list=ls())

setwd("~/Documents/Documents/R/Statcourse/BIOS14")

library(Hmsc)

x = rnorm(200, 10, 3)
y = -2 + 0.4*x + rnorm(200, 0, 2)
m1 = lm(y~x)

#As a first very simple example, we will fit a simple linear regression to 
#simulated data. While the lm function constructs and fits the model in one go,
#the Hmscpackage first constructs the model, and then performs
#model fitting (posterior sampling) in a second step. This is because
#posterior sampling can take a long time for complex data, and we want to be 
#able to leave it to run e.g. overnight. For the current model though,
#model fitting is very quick.


m2 = Hmsc(Y = as.matrix(y), XData = data.frame(x), XFormula = ~x,
          distr="normal") # Construct model

m2 = sampleMcmc(m2, samples=1000, transient=1000, thin=1,
                nChains=2, verbose=F)  #Model fitting
#thin = how often a sample is taken, sample= how many samples to include, 
#transient = how many samples that are cut off in the begining


summary(m1)$coef

mpost = convertToCodaObject(m2)
summary(mpost$Beta)

#The parameter estimates are very similar, though not identical. 
#This is due to the stochasticity of the MCMC algorithm. The fact that we 
#have sampled the posterior distribution with MCMC also means that,
#before we start looking more in detail at the model estimates, 
#we should assess whether the model actually converged on a stable solution. 
#One way to do this is to produce a posterior trace plot, which shows how
#the parameter estimates have changed during the MCMC chain.

plot(mpost$Beta)

#Here the posterior trace plot looks fine, there is no directional trend and 
#the posterior distribution is nicely bell-shaped. We can also evaluate 
#whether the chain has mixed (explored the parameter space) well by
#computing the effective sample size (which should be close to the number 
#of posterior samples).

effectiveSize(mpost$Beta)

#Because we ran two independent MCMC chains, we can also assess whether
#they yielded similar results, which can be quantified by the so-called 
#potential scale reduction factor or the Gelman-Rubin criterion.
#Values close to 1 means that the chains yielded similar results.

gelman.diag(mpost$Beta, multivariate=F)$psrf


#One advantage of Bayesian analyses is that, because we obtain the entire 
#posterior distribution, we can carry uncertainty in parameter estimates
#forward to subsequent steps in the analysis. As a simple example,
#if we are using the parameter estimates of the simple linear model above 
#to make predictions about the value of y for some values of x, we can obtain 
#those predicts across the entire posterior disteribution, and
#thus construct e.g. a confidence interval for the predictions. Because 
#‘confidence intervals’ are well defined in standard maximum-likelihood 
#statistics, Bayesian refer instead to ‘credible intervals’, often so-called 
#‘highest posterior density’ (HPD) intervals.

#Fitting a linear mixed model #####

library(Hmsc)
library(glmmTMB)

set.seed(145)
x1 = rnorm(200, 10, 2)
groupmeans = rep(rnorm(10, 20, 4), each=20)
groupID = as.factor(rep(paste0("Group", 1:10), each=20))
y = 2 + 1.5*x1 + groupmeans + rnorm(200, 0, 2)

m1 = glmmTMB(y~x1 + (1|groupID)) #Model fitted with frequency statistics


studyDesign = data.frame(group = as.factor(groupID))
rL1 = HmscRandomLevel(units = groupID) #Set group as random factor
m2 = Hmsc(Y = as.matrix(y), XData = data.frame(x1), XFormula = ~x1,
          studyDesign = studyDesign, ranLevels = list(group = rL1),
          distr="normal") #Construct model with Bayesian method

m2 = sampleMcmc(m2, samples=1000, transient=1000, thin=1,
                nChains=2, verbose=F) #Fit model with Bayesian method

#For the glmmTMB model, we get the parameter estimates and random-effect
#variances from the summary function. For the Hmsc model, 
#we get the parameter estimates and variance explained by the random effect
#(group) separately (the latter is called Omega in Hmsc).

summary(m1)

mpost = convertToCodaObject(m2)
summary(mpost$Beta)

getPostEstimate(m2, "Omega")$mean #Variance explained by group









