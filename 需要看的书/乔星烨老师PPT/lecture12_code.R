install.packages('CVXR')
install.packages('MASS')
install.packages('naivebayes')
install.packages('Metrics')
install.packages('e1071')

suppressWarnings(library(CVXR, warn.conflicts=FALSE))
library(naivebayes)
library(MASS)
library(Metrics)
library(e1071)


## non-separable case

set.seed(123)
var <- 5.6
n <- 100
mu1 <- c(3,8)
mu2 <- c(8,3)
## simulate bivariate normal data 
x1 <- mvrnorm((n/2),mu1,diag(rep(var,2)))
x2 <- mvrnorm((n/2),mu2,diag(rep(var,2)))
x <- rbind(x1,x2)
## simulate the binary response 
Y <- matrix(rep(c(1,-1),each=(n/2)),n,1)

## visualize the data 
plot(c(min(x[,1]),max(x[,1])),c(min(x[,2]),max(x[,2])),type='n',xlab='Feature 1',ylab='Feature 2')
points(x1[,1],x1[,2],col='red',pch=21,bg='red')
points(x2[,1],x2[,2],col='blue',pch=22,bg='blue')

gam <- 5


## solve the primal form using cvx
betaHat <- Variable(2)
beta0 <- Variable(1)
epsilon <- Variable(n)
objective <- Minimize(0.5*sum(betaHat^2) + gam*sum(epsilon))
constraint1 <- Y*(x%*%betaHat + beta0) >= 1-epsilon
constraint2 <- epsilon >= 0
problem <- Problem(objective, constraints = list(constraint1,constraint2))
result <- solve(problem)

# save the optimal beta0 and betas
betas_primal <- result$getValue(betaHat)
beta0_primal <- result$getValue(beta0)
epsilon <- result$getValue(epsilon)

## solve the dual form using cvx
alpha <- Variable(n)
objective <- Maximize( -0.5*quad_form(Y*alpha,(x%*%t(x))) + sum(alpha) )
constraint1 <- sum(Y*alpha) == 0
constraint2 <- alpha >= 0
constraint3 <- alpha <= gam
problem <- Problem(objective, constraints = list(constraint1,constraint2,constraint3))
result <- solve(problem)

# save the optimal beta0 and betas
alpha <- result$getValue(alpha)
betas_dual <- colSums( as.numeric(Y*alpha)*x )

ind = (alpha > 10^-6) & (alpha < gam-10^-6)
(beta0_dual = - x[ind,]%*%as.matrix(betas_dual) + 1/Y[ind])

## compare the primal and dual soltuions.
cbind(primal = betas_primal,dual = betas_dual)
c(beta0_primal,mean(beta0_dual))



###############################
## non-linear 2-class SVM

set.seed(123)

var <- 1.25
n <- 100
mu1 <- c(0,0)
cov1 <- c(1.8,1.8)
## simulate non-linear data 
x1 <- mvrnorm((n/2),mu1,(matrix(c(1,0,0.8,0.4),2,2)*var))
xs2 <- mvrnorm((n*100),mu1,diag(rep((var*5),2)))
ind <- apply(xs2^2,1,sum) > 0.5
x2 <- xs2[ind,]
x2 <- x2[1:(n/2),]
x <- rbind(x1,x2)
## simulate the binary response 
Y <- matrix(rep(c(1,-1),each=(n/2)),n,1)

## visualize the data 
dev.new()
plot(c(min(x[,1]),max(x[,1])),c(min(x[,2]),max(x[,2])),type='n',xlab='Feature 1',ylab='Feature 2')
points(x1[,1],x1[,2],col='red',pch=21,bg='red')
points(x2[,1],x2[,2],col='blue',pch=22,bg='blue')

## put data in to a data frame 
dat <- data.frame(V1=x[,1],V2=x[,2],label=as.factor(Y))


## linear SVM
## C = 1
fitsvm.linear <- svm(label~., data = dat, type = 'C-classification',
              kernel = 'linear')
plot(fitsvm.linear, dat, V1~V2)

## radial kernel 
## C = 1, gamma = 1
fitsvm.rbf <- svm(label~., data = dat, type = 'C-classification',
              kernel = 'radial', gamma = 1,cost=1)
plot(fitsvm.rbf, dat, V1~V2)

## polynomial kernel
## d = 2, C = 0.5

fitsvm.poly <- svm(label~., data = dat, type = 'C-classification',
                  kernel = 'polynomial', degree = 2, cost = 1)
plot(fitsvm.poly, dat, V1~V2)



##############
## non-linear regression: GAM

install.packages('mgcv')
install.packages('genlasso')
install.packages('CVST')
library(mgcv)
library(genlasso)
library(CVST)
library(e1071)

#load data from the R package (if you don't have the .csv file)
library(cosso)
data(ozone)
#OR load data from .csv file
ozone = read.csv("ozone.csv")

## fit a gam model
fitg = gam(ozone~s(temp)+s(invHt)+s(press)+s(vis)+s(milPress)+s(hum)+s(invTemp)+s(wind),data=ozone)
summary(fitg)
plot(fitg,shade=TRUE)



################
## trend filering 
a = trendfilter(y=ozone$temp,ord=1)
plot(a,nlam=1,ylab='Temp')

#another example
# Cubic trend filtering
set.seed(0)
n = 100
beta0 = numeric(100)
beta0[1:40] = (1:40-20)^3
beta0[40:50] = -60*(40:50-50)^2 + 60*100+20^3
beta0[50:70] = -20*(50:70-50)^2 + 60*100+20^3
beta0[70:100] = -1/6*(70:100-110)^3 + -1/6*40^3 + 6000
beta0 = -beta0
beta0 = (beta0-min(beta0))*10/diff(range(beta0))
y = beta0 + rnorm(n)
a = trendfilter(y,ord=3)
plot(a,nlam=5)


##################
##################
## kernel ridge


## simulate data
set.seed(123)
n <- 200
p <- 1
x <- matrix(runif(n),n,1)*10
y <- sin(x)*4 + matrix(rnorm(n),n,1)

plot(x,y,pch=16,cex=1)

##linear - k=x'x
betals <- solve(t(x)%*%x)%*%t(x)%*%y
xs <- matrix(seq(0,10,length.out = 500),500,1)
plot(x,y,pch=16,cex=1)
lines(xs,xs%*%betals,col=4)

##polynomial - k=(x'x)^d, d = 2, lam = 2
krr.data <- constructData(x, as.numeric(y))
p <- list(kernel="polydot", degree=2, lambda=2)
krr <- constructKRRLearner()
m <- krr$learn(krr.data, p)
pred <- krr$predict(m, krr.data)
plot(x,y,pch=16,cex=1)
lines(x[order(x)],pred[order(x)],col='red', lwd = 3)


##polynomial - k=(x'x)^d, d = 4, lam = 2
krr.data <- constructData(x, as.numeric(y))
p <- list(kernel="polydot", degree=4, lambda=2)
krr <- constructKRRLearner()
m <- krr$learn(krr.data, p)
pred <- krr$predict(m, krr.data)
plot(x,y,pch=16,cex=1)
lines(x[order(x)],pred[order(x)],col='red',lwd=4)

##polynomial - k=(x'x)^d, d = 4, lam = 0.1
krr.data <- constructData(x, as.numeric(y))
p <- list(kernel="polydot", degree=4, lambda=0.1)
krr <- constructKRRLearner()
m <- krr$learn(krr.data, p)
pred <- krr$predict(m, krr.data)
plot(x,y,pch=16,cex=1)
lines(x[order(x)],pred[order(x)],col='red',lwd=4)

## Gaussian RBF
krr.data <- constructData(x, as.numeric(y))
p <- list(kernel="rbfdot", sigma=1, lambda=.1/getN(krr.data))
krr <- constructKRRLearner()
m <- krr$learn(krr.data, p)
pred <- krr$predict(m, krr.data)
plot(x,y,pch=16,cex=1)
lines(x[order(x)],pred[order(x)],col='red',lwd=3)


## trend filering
ind = order(x)
a = trendfilter(y[ind],pos=x[ind],ord=4)
plot(a,nlam=5,pch=16)


