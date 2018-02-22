#
# Multivariate analysis
#
# Introduction to multivariate analysis with R.
#
# Jan Graffelman, February 2018
#
# Topics
#
# 1. Basic operations.
# 2. Loops and conditional execution.
# 3. Reading and writing files. 
# 4. Solving linear equations.
# 5. Inverse, rank, determinant.
# 6. Eigenvalues and eigenvectors.
# 7. Data structures in R.
# 8. Generating random data.
# 9. Probability distributions.
#10. Scripts and functions
#11. Graphics.
#12. Singular value decomposition.
#13. Basic matrices in multivariate analysis.

#
# 1. Basic operations.
#

3*4
a <- 2*3
ls()
r <- 2
b <- pi*r^2
b

A <- matrix(c(1,2,3,4),ncol=2)
A
B <- matrix(c(4,5,6,7),ncol=2)
B

# matrix multiplication

A%*%B

# elementwise multiplicatioin

A*B

# transpose

t(A)
At <- t(A)
At

b <- c(1,1)
A%*%b

# make a diagonal matrix

D <- diag(c(1,2,3))
D
M <- matrix(1:9,ncol=3)
M
M <- matrix(1:9,ncol=3,byrow=TRUE)
M
D <- diag(diag(M))
D

# Identity, zero, and ones matrix.

I <- diag(rep(1,4))
I
O <- matrix(rep(0,4*4),ncol=4)
O
J <- matrix(rep(1,4*4),ncol=4)
J

#
# 2. Loops and conditional execution.
#

library(MASS)
data(cats)
cats
?cats
class(cats)
colnames(cats)
x <- cats[,2]
n <- length(x)
m <- mean(x)
ML <- TRUE
if(ML) vx <- (1/n)*sum((x-m)^2) else (1/(n-1))*sum((x-m)^2)
vx
var(x)
(n/(n-1))*vx

# loops

x <- runif(20)
x
n <- length(x)
n
rootx <- numeric(n)
rootx

for(i in 1:n) {
  rootx[i] <- sqrt(x[i])
}
rootx

# more efficient

y <- sqrt(x)
y

# loop to solve x*exp(-x) = 0.25

xinit <- 10
x <- xinit
err <- 10
while(err > 0.00001) {
   hx <- (0.25 - x*exp(-x))/(exp(-x)*(1-x))
   xn <- x + hx
   err <- abs(hx)
   cat(x,hx,"\n")
   x <- xn
}
print(x)
x*exp(-x)

#
# 3. Reading and writing files. 
#

# reading a text file from disc

X <- read.table("e:/Goblets.dat")
X

# reading a file directly from the web

X <- read.table("http://www-eio.upc.es/~jan/data/Goblets.dat",header=TRUE)
X <- as.matrix(X)
X

# reading a CSV file made by Excel

X <- read.csv("http://www-eio.upc.es/~jan/data/Goblets.csv",header=TRUE,sep=";")
X <- as.matrix(X)
X

# reading data from Window's clipboard

X <- read.table("clipboard",header=TRUE)
X <- as.matrix(X)
X

# reading data from an SPSS file

library(foreign)
X <- read.spss("e:/goblets.sav")
X

# reading directly from an EXCEL file (.xls,.xlsx)

#install.packages("gdata")
#library(gdata)
#X <- read.xls("http://www-eio.upc.es/~jan/data/goblets.xlsx")

n <- nrow(X)
n
p <- ncol(X)
p

# writing a text file

write(t(X),file="e:/MyData.dat",ncolumns=6)

# writing binary files

save(X,A,B,file="e:/MyThings.rda")
rm(list=ls())
ls()
load("e:/MyThings.rda")
ls()

#
# 4. Solving linear equations.
#

A <- matrix(runif(9),ncol=3)
A

# We make b a column of ones

b <- rep(1,3)
b

x <- solve(A,b)
print(x)

A%*%x

# We make a singular matrix

B <- cbind(A[,1:2],A[,2])
print(B)
solve(B)

# Computing the inverse of A

Ai <- solve(A)
print(Ai)
Ai%*%A
A%*%Ai
round(A%*%Ai,digit=6)

# Calculating a determinant

det(A)
det(B)

# calculate the rank of a matrix

A <- t(A)%*%A
print(A)
Res <- eigen(A)
Res

# the rank is the number of non-zero eigenvalues 

#
# 6. Eigenvalues and eigenvectors.
#
# We do the spectral decomposition

class(A)
class(Res)

Dlam <- diag(Res$values)
Dlam

V <- Res$vectors
V

# verify the decomposition 

V%*%Dlam%*%t(V)
A

# and the orthogonality of V

t(V)%*%V
round(t(V)%*%V,digits=6)

#
# 7. Data structures in R.
#

a <- pi
class(a)

b <- runif(10)
class(b)

class(A)
class(Res)

E <- data.frame(A,c("D","D","H"))
colnames(E) <- c("V1","V2","V3","Sex")
E
class(E)

class(Res[2])
class(Res[[2]])

# create a list

X <- list(A=A,E=E)
X
X$A
X$E

#
# 8. Generating random data.
#

x <- runif(10)
x
y <- rnorm(1000)
y
qqnorm(y)
qqline(y)

mean(y)
var(y)
y <- rexp(1000)
qqnorm(y)
qqline(y)

# calculate a percentile

qnorm(0.5)

# calculate the distribution function

pnorm(0)

# calculate the value of the density function

dnorm(0)

# other distributions
#
# rt, rf, rchisq, rgamma, rbeta, rbinom, rpois, etc.
#

#
# 10. Scripts and functions
#

source("W:/script.R")

#X <- read.table("W:/Goblets.dat")
#X
X <- read.table("http://www-eio.upc.es/~jan/data/Goblets.dat",header=TRUE)
X <- as.matrix(X)


m <- apply(X,2,mean)
m

Describe <- function(x) {
   m <- mean(x)
   n <- length(x)
   s <- sqrt((1/(n-1))*sum((x-m)*(x-m)))
   return(list(m=m,s=s))
}

res <- Describe(X[,1])
res$m
res$s

mean(X[,1])
sqrt(var(X[,1]))

#
# 11. Graphics
#

hist(X[,2])
plot(X[,1],X[,2],pch=19)
plot(runif(100),runif(100),pch=19,asp=1)
pairs(X)
boxplot(X[,2])
boxplot(X)

#
# 12. Singular value decomposition.
# 

class(X)
X <- as.matrix(X)
class(X)

Dec <- svd(X)
Dec

U <- Dec$u
D <- diag(Dec$d)
V <- Dec$v

U%*%D%*%t(V)

Xapprox <- U[,1:2]%*%D[1:2,1:2]%*%t(V[,1:2])
X
Xapprox

eigen(t(Xapprox)%*%Xapprox)$values

E <- X-Xapprox
E

sum(diag(t(E)%*%E))

DD <- D*D
sum(diag(DD)[3:nrow(DD)])

#
# 13. Basic matrices in multivariate analysis.
#

S <- cov(X)
S
R <- cor(X)
R

m <- apply(X,2,mean)
Xc <- X - rep(1,nrow(X))%o%m
apply(Xc,2,mean)
Xs <- Xc%*%diag(1/sqrt(diag(cov(X))))
Xs
apply(Xs,2,mean)
apply(Xs,2,var)

n <- nrow(X)
p <- ncol(X)

In <- diag(rep(1,n))
H <- In - (1/n)*rep(1,n)%o%rep(1,n)
H

Xc[1:4,]

Xt <- H%*%X
Xt[1:4,]

S
(1/(n-1))*t(Xc)%*%Xc

Ds <- sqrt(diag(diag(S)))
Dsinv <- solve(Ds)

Dsinv%*%S%*%Dsinv
R

# Euclidean distance matrix

De <- as.matrix(dist(X))
De[1:5,1:5]
