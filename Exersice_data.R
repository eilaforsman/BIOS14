setwd("~/Documents/Documents/R/Statcourse/Exersice 1")

x = rnorm(n=100, mean=5, sd=1)
mean(x)
sd(x)
hist(x, las=1, main="")

set.seed(1)
x = rnorm(50, 10, 2)
se_x = sqrt(var(x)/length(x))

out = NULL
for(i in 1:1000){
  sample = sample(x, replace=TRUE)
  out[i] = mean(sample)
}
hist(out, las=1, main="")
sd(out)
se_x
quantile(out, c(0.025, 0.975))#c()Extract value for 0.025 and 0.975
mean(x) - 1.96*se_x
mean(x) + 1.96*se_x

#EXERSICE####

set.seed(1)
x = rnorm(50, 10, 2)
se_x = sqrt(var(x)/length(x))