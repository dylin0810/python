########################
#lecture 11 code

###############
#optimal separating hyperplanes & linear SVMs
install.packages('CVXR')
suppressWarnings(library(CVXR, warn.conflicts=FALSE))
library(MASS)

## change this to your cvx directory on your system 
setup.dir <- "~/Desktop/cvx"

## separable case

set.seed(123)
var <- 1.25
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

suppressWarnings(library(CVXR, warn.conflicts=FALSE))


## solve the primal form using cvx
betaHat <- Variable(2)
beta0 <- Variable(1)
objective <- Minimize(sum(betaHat^2))
constraint1 <- Y*(x%*%betaHat + beta0) >= 1
problem <- Problem(objective, constraints = list(constraint1))
result <- solve(problem)

## save the optimal beta0 and betas
betas <- result$getValue(betaHat)
beta0 <- result$getValue(beta0)


## visualize the optimal separating hyperplane
plot(c(min(x[,1]),max(x[,1])),c(min(x[,2]),max(x[,2])),type='n',xlab='Feature 1',ylab='Feature 2')
points(x1[,1],x1[,2],col='red',pch=21,bg='red')
points(x2[,1],x2[,2],col='blue',pch=22,bg='blue')
xs <- seq(min(x),max(x),length.out = 1000)
lines(xs,((-beta0-xs*betas[1])/betas[2]),lwd=2)
lines(xs,((-beta0-xs*betas[1]+1)/betas[2]),lty=3,lwd=2)
lines(xs,((-beta0-xs*betas[1]-1)/betas[2]),lty=3,lwd=2)
## the margin of the separation 
(M <- 1/norm(betas,type='2'))

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

## save the optimal beta0 and betas
betas <- result$getValue(betaHat)
beta0 <- result$getValue(beta0)
epsilon <- result$getValue(epsilon)

## visualize the optimal separating hyperplane
plot(c(min(x[,1]),max(x[,1])),c(min(x[,2]),max(x[,2])),type='n',xlab='Feature 1',ylab='Feature 2')
points(x1[,1],x1[,2],col='red',pch=21,bg='red')
points(x2[,1],x2[,2],col='blue',pch=21,bg='blue')
xs <- seq(min(x),max(x),length.out = 1000)
lines(xs,((-beta0-xs*betas[1])/betas[2]),lwd=2)
lines(xs,((-beta0-xs*betas[1]+1)/betas[2]),lty=3,lwd=2)
lines(xs,((-beta0-xs*betas[1]-1)/betas[2]),lty=3,lwd=2)
ind <- epsilon > 10^-6
points(x[ind,1],x[ind,2],pch=3,cex=3)
ind <- abs(as.numeric(x%*%matrix(betas,2,1)) + beta0 + 1) < 10^-4
points(x[ind,1],x[ind,2],pch=4,cex=3)
ind <- abs(as.numeric(x%*%matrix(betas,2,1)) + beta0 - 1) < 10^-4
points(x[ind,1],x[ind,2],pch=4,cex=3)
## the margin of the separation 
(M <- 1/norm(betas,type='2'))


#################################
## comparison of NB, LDA, and linear SVM using zip code data

library(naivebayes)
library(MASS)
library(Metrics)
library(e1071)


## import the data
zip1 <- 3
zip2 <- 8
dat <- read.table('zip.train')
dat <- subset(dat, V1 %in% c(zip1,zip2))
x <- as.matrix(dat[,2:dim(dat)[2]])
n <- dim(x)[1]
p <- dim(x)[2]

tdat <- read.table('zip.test')
tdat <- subset(tdat, V1 %in% c(zip1,zip2))
tx <- as.matrix(tdat[,2:dim(tdat)[2]])
tn <- dim(tx)[1]
tp <- dim(tx)[2]


## plot the average image of each digit
colors<-c('white','black')
cus_col<-colorRampPalette(colors=colors)

par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
all_img<-array(dim=c(10,16*16))
for(i in 0:9)
{
  all_img[i+1,]<-apply(dat[dat[,1]==i,-1],2,sum)
  all_img[i+1,]<-all_img[i+1,]/max(all_img[i+1,])*255
  
  z<-array(all_img[i+1,],dim=c(16,16))
  z<-z[,16:1] 
  image(1:16,1:16,z,main=i,col=cus_col(256),xlab='',ylab='')
}


## C = 1
fitsvm <- svm(factor(V1)~., data = dat, type = 'C-classification',
              kernel = 'linear')

(svm.training.error <- ce(as.factor(dat[,1]),predict(fitsvm,newdata = x))) # overfitting ! 
(svm.test.error <- ce(as.factor(tdat[,1]),predict(fitsvm,newdata = tx)))

## plot some of the support vectors
sv.ind <- sample(seq(1,dim(fitsvm$SV)[1]),16)
par(mfrow=c(4,4),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
all_img<-array(dim=c(16,16*16))
for(i in sv.ind)
{
  z<-array(fitsvm$SV[i,],dim=c(16,16))
  z<-z[,16:1] 
  image(1:16,1:16,z,main=i,col=cus_col(256),xlab='',ylab='')
}


## compare test error to other methods
## use Naive Bayes classifier 
NB <- naive_bayes(x=x,y=as.factor(dat[,1]))
(NB.test.error <- ce(as.factor(tdat[,1]),predict(NB,tx,type='class')))

## use built-in LDA function
LDA <- lda(x,as.factor(dat[,1]))
(LDA.test.error <- ce(as.factor(tdat[,1]),predict(LDA,tx)$class))
