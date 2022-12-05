rm(list=ls())

setwd("~/Documents/Documents/R/Statcourse/BIOS14")

#Multivariate exersice#####

#Multivariate data can be summarized as variance matrices, which are symmetrical 
#matrices with variances on the diagonal and covariances on the off-diagonals.

cm = matrix(c(0.7, 0.2, -0.3,
              0.2, 1.2, 0.4,
              -0.3, 0.4, 0.6),
            nrow=3)
cm

#One way to confirm that a matrix is symmetrical is to show that it is identical 
#to it ́s transpose (A = A^T). Confirm that CM is symmetrical. To transpose a 
#matrix in R we can use the t function.

cm == t(cm)

#Correlations are standardized covariances, i.e. they are the covariances 
#between standardized variables (mean= 0, sd = 1). We can compute the 
#correlation between two variables as Cor(x,y) = Cov(x,y)/√V ar(x)V ar(y).

#Translate the covariance matrix into a correlation matrix.

cov2cor(cm)

#We can use a variance matrix to simulate data from the multivariate normal 
#distribution MVN( ̄x,Σ), where Σ is a variance matrix.

library(MASS)
library(ellipse)

set.seed(1)
X = data.frame(mvrnorm(200, mu=c(0,0,0), Sigma=cm))
colnames(X) = c("z1", "z2", "z3")
head(X)

means = c(apply(X[,1:2], 2, mean))
plot(X$z1, X$z2, las=1)
lines(ellipse(cov(X[,1:2]), centre=means))

#Variance matrices have several useful properties for multivariate analysis.
#A common operation is a so-called eigendecomposition (or spectral decomposition).

#A vector v is an eigenvector of the matrix A if it satisfies the condition
#Av = λv,
#where λ is an eigenvalue of A. From this follows also the relation
#A = QΛQ^−1.
#where Q is a matrix with the eigenvectors in columns, and Λ is a square matrix 
#with the eigenvalues on the diagonal.

eigen(cm)

#The eigenvalues represent the amount of variance associated with each eigenvector 
#(given in columns). We can thus compute the proportion of variance associated
#with each eigenvector as λi/∑ λ.

plot(X$z1, X$z2, las=1, col="grey")
lines(ellipse(cov(X[,1:2]), centre=means))
arrows(means[1], means[2],
       means[1]+eigen(cm)$vectors[1,1],
       means[2]+eigen(cm)$vectors[2,1],
       code=2, length=0.1, lwd=2)
arrows(means[1], means[2],
       means[1]+eigen(cm)$vectors[1,2],
       means[2]+eigen(cm)$vectors[2,2],
       code=2, length=0.1, lwd=2)


#There are several forms of matrix multiplication, but the ‘normal’ matrix 
#multiplication requires that the number of columns in the first
#matrix equals the number of rows in the second matrix, and the resulting 
#matrix will have the same number of rows as the first matrix, and the same 
#number of columns as the second matrix. If we multiply a matrix of
#dimensions m×n with one of dimensions n×l, we get a matrix of dimensions m×l.

#Compute the proportion of variance associated with each eigenvector of CM.

eigen(cm)$values/sum(eigen(cm)$values)

#Confirm that the eigenvectors are of unit length (length = 1) and that the 
#angle between them is 90 degrees.
#Recall that the length of a vector is the square root of the sum of the 
#vector elements, and the angle between two vectors u1 and u2 is 180/π cos−1(u1u2).


sqrt(sum(eigen(cm)$vectors))
apply(eigen(cm)$vectors, 2, function(x) sqrt(sum(x^2)))
#[1] 1 1 1

180/pi * acos(t(eigen(cm)$vectors[,1]) %*% eigen(cm)$vectors[,2]) 
#t transforms 1 column to a row in order for multiplication of matrix to work
#    [,1]
#[1,]   90

#Reconstruct the matrix CM from the eigenvalues and eigenvectors.

eigen(cm)$vectors %*% diag(eigen(cm)$values) %*% solve(eigen(cm)$vectors)

eigen(cm)$vectors[1:2, 1:2] %*% 
  diag(eigen(cm)$values)[1:2, 1:2] %*% 
  solve(eigen(cm)$vectors)[1:2, 1:2]

#PCA####

#Eigenanalysis is a core component of principal component analysis. In it’s 
#simplest form, the principal components are the same as the eigenvectors. 
#Let us derive some new traits along the eigenvectors of CM.

dim(as.matrix(X))
dim(as.matrix(eigen(cm)$vectors[,1]))

t1 = as.matrix(X) %*% eigen(cm)$vectors[,1]
t2 = as.matrix(X) %*% eigen(cm)$vectors[,2]
t3 = as.matrix(X) %*% eigen(cm)$vectors[,3]

c(var(X[,1]), var(X[,2]), var(X[,3]))
c(var(t1), var(t2), var(t3))

#Notice that the variances of new traits decreases from the first to the 
#third trait, which was not the case for the original traits. However, 
#the total variance stays the same (because we have just reorganized the variation).

var(t1) + var(t2) + var(t3)
var(X[,1]) + var(X[,2]) + var(X[,3])

#The eigenvectors are orthogonal, i.e. they are not correlated with each other.

pca = princomp(X)
summary(pca)

#The principal components are not exactly the same as defining traits along 
#the eigenvectors, but are subject to some further rotation. However, 
#the principal components will be strongly correlated with the traits
#defined along the eigenvectors.

#The proportion of variance explained by each principal component is computed 
#as the variance of each principal component divided by the total, which is
#basically equal to the corresponding eigenvalue divided
#by the sum of the eigenvalues, i.e. λi/∑ λ.

pca$sdev^2/sum(pca$sdev^2)
eigen(cm)$values/sum(eigen(cm)$values)

#The small difference is again due to how the principal components are
#calculated, but biologically the interpretation is the same

#To understand how each principal component is constructed, we look at the 
#loadings of each original variable onto the PCs.

pca$loadings[1:3, 1:3]

#Here, the first PC represents variation in z2 and, to a lesser extent, 
#z3 (the two variables must thus be positively correlated). The second PC 
#represents mostly z1, and also separates variation in z2 from variation in z3.

#A PCA can also be illustrated by a biplot.

biplot(pca, col=c("grey", "black"), cex=c(.5, 1))

#Data Exercise#####

rm(list=ls())
dat = read.csv("blossoms.csv")












