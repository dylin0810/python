###########################
#lecture 6 code

require(glmnet)
require(Hmisc)

#model complexity & prediction error

set.seed(9001)

n = 100
p = 50
Btrue = matrix(0,p,1)
Btrue[1:10] = rnorm(10)*1; 

Xtr = scale(matrix(rnorm(n*p),n,p))
Ytr = Xtr%*%Btrue + matrix(rnorm(n),n,1)
Xts = scale(matrix(rnorm(n*p),n,p))
Yts = Xts%*%Btrue + matrix(rnorm(n),n,1)

fit = glmnet(x=Xtr,y=Ytr,family="gaussian",standardize=FALSE,nlambda=100,lambda.min.ratio=.001)
Yhtr = predict(fit,newx=Xtr)
MSEtr = apply((Yhtr-Ytr%*%matrix(1,1,length(fit$lambda)))^2,2,mean)
Yhts = predict(fit,newx=Xts)
MSEts = apply((Yhts-Yts%*%matrix(1,1,length(fit$lambda)))^2,2,mean)

plot(fit$lambda,MSEtr,type="n",col=4,xlab="Lambda",ylab="Error",log="x",xlim=c(max(fit$lambda),min(fit$lambda)))
lines(fit$lambda,MSEtr,col=4)
lines(fit$lambda,MSEts,col=2)
legend(.01,10,legend=c("Training Error","Test Error"),col=c(4,2),lty=c(1,1),cex=.75)


#Cross-Validation (by hand)
#Model Selection

fold = 5
sam = sample(1:n,n)
CVerrs = NULL
for(i in 1:fold)
{
  ind = sam[((i-1)*n/fold + 1):(i*n/fold)]
  Xin = Xtr[-ind,]; Yin = Ytr[-ind]
  Xout = Xtr[ind,]; Yout = Ytr[ind]
  fitcv = glmnet(x=Xin,y=Yin,family="gaussian",standardize=FALSE,lambda=fit$lambda)
  Yh = predict(fitcv,newx=Xout)
  CVerrs = cbind(CVerrs,apply((Yh-Yout%*%matrix(1,1,length(fitcv$lambda)))^2,2,mean))
}
CVerr = apply(CVerrs,1,mean)

#minimum CV error rule
lines(fit$lambda,CVerr,col=1)
legend(.01,10,legend=c("Training Error","Test Error","CV Error"),col=c(4,2,1),lty=c(1,1,1),cex=.75)
optlam = fit$lambda[which.min(CVerr)]
sp = fit$df[which.min(CVerr)]
lines(c(optlam,optlam),c(0,15),col=1,lty=2)
sp

#refit to training data at optimal lambda
fitn = glmnet(x=Xtr,y=Ytr,family="gaussian",standardize=FALSE,lambda=optlam)
Yhtr = predict(fitn,newx=Xtr)
TRerr = mean( (Yhtr - Ytr)^2 )
Yhts = predict(fitn,newx=Xts)
TSerr = mean( (Yhts - Yts)^2 )
TRerr
TSerr


#one SE rule
SE = sqrt(apply(CVerrs,1,var)/fold)
errbar(fit$lambda,CVerr,CVerr+SE,CVerr-SE,add=TRUE)
minSE = CVerr[which.min(CVerr)]+SE[which.min(CVerr)]
minSEx = which(CVerr<minSE)[1]
optlamSE = fit$lambda[minSEx]
sp = fit$df[minSEx]
sp
lines(c(optlamSE,optlamSE),c(0,15),col=1,lty=3)

#refit to training data at optimal lambda
fitn = glmnet(x=Xtr,y=Ytr,family="gaussian",standardize=FALSE,lambda=optlamSE)
Yhtr = predict(fitn,newx=Xtr)
TRerr = mean( (Yhtr - Ytr)^2 )
Yhts = predict(fitn,newx=Xts)
TSerr = mean( (Yhts - Yts)^2 )
TRerr
TSerr

#why doesn't CV (without the 1-SE rule) find the true model?


############################
#CV for Lasso on ozone data

#load data from the R package (if you don't have the .csv file)
library(cosso)
data(ozone)
#OR load data from .csv file
ozone = read.csv("ozone.csv")

Y = as.numeric(ozone[,1]); Y = Y - mean(Y)
X = as.matrix(ozone[,-1]); X = scale(X,center=T,scale=T)

#run 10-fold CV using built-in function
cvfit = cv.glmnet(X,Y)
plot(cvfit)
cvfit$lambda.min
cvfit$lambda.1se

#re-run Lasso at optimal lambda
fit = glmnet(x=X,y=Y,family="gaussian",lambda=cvfit$lambda.1se,alpha=1)
as.matrix(fit$beta)



