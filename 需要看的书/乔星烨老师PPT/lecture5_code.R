###########################
#lecture 5 code
#ozone data

library(glmnet)

#load data from the R package (if you don't have the .csv file)
library(cosso)
data(ozone)
#OR load data from .csv file
ozone = read.csv("ozone.csv")

############################
#L1 Regularization

Y = as.numeric(ozone[,1]); Y = Y - mean(Y)
X = as.matrix(ozone[,-1]); X = scale(X,center=T,scale=T)
fit0 = lm(Y~X-1)


lam = 1
fitl = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=1)
cbind(fit0$coef,as.matrix(fitl$beta))


#lasso paths
fitl = glmnet(x=X,y=Y,family="gaussian",alpha=1)
plot(fitl,col=1:8)
legend(0,4,legend=names(ozone)[2:9],col=1:8,lty=rep(1,8),cex=.8)


###############################
#least squares, lasso, adaptive lasso, ridge, elastic net

lam = 1

## ols
betals = solve(t(X)%*%X)%*%t(X)%*%Y
## ridge
betar = solve(t(X)%*%X + diag(rep(lam/2*nrow(ozone),8)))%*%t(X)%*%Y
## lasso
fitl = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=1)
## adaptive lasso
fital = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=1,penalty.factor=1/abs(betals))
## elastic net
fitel = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.5)
mat = cbind(betals,betar,as.matrix(fitl$beta),as.matrix(fital$beta),as.matrix(fitel$beta))
colnames(mat) = c("LS","Ridge","Lasso","A-Lasso","EL")
mat

#############################
#compare ridge, lasso & elastic net regualrization paths

par(mfrow=c(2,2))
par(mar=c(5,4,3,2))
betals = solve(t(X)%*%X)%*%t(X)%*%Y
lambdas = exp(seq(log(.01),log(100*nrow(ozone)),l=50))
betasr = matrix(0,length(lambdas),8)
for(i in 1:length(lambdas))
{
  betasr[i,] = solve(t(X)%*%X + diag(rep(lambdas[i],8)))%*%t(X)%*%Y
}
plot(c(1,length(lambdas)),range(betals),type="n",ylab="Coefficients",xlab="Lambda Index",main="Ridge")
for(j in 1:8)
{
  lines(betasr[length(lambdas):1,j],col=j)
}
legend(0,4,legend=names(ozone)[2:9],col=1:9,lty=rep(1,9),cex=.75)

fitl = glmnet(x=X,y=Y,family="gaussian",alpha=1)
plot(fitl,col=1:8,main="Lasso")
legend(0,4,legend=names(ozone)[2:9],col=1:8,lty=rep(1,8),cex=.75)

fitel = glmnet(x=X,y=Y,family="gaussian",alpha=.5)
plot(fitel,col=1:8,main="EL alpha=.5")
legend(0,4,legend=names(ozone)[2:9],col=1:8,lty=rep(1,8),cex=.75)

fitel = glmnet(x=X,y=Y,family="gaussian",alpha=.25)
plot(fitel,col=1:8,main="EL alpha=.25")
legend(0,4,legend=names(ozone)[2:9],col=1:8,lty=rep(1,8),cex=.75)







