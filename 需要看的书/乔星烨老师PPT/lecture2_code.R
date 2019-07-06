#########################
#lecture 2 code
#ozone data

#load in ozone data (taken from "cosso" R package)
#ozone = read.csv("ozone.csv")

#take a look at the data
plot(ozone,pch=16,cex=.5)

#least squares using the lm() function
fit = lm(ozone~.,data=ozone)
summary(fit)
#plot(fit)

#directly fit least squares
Y = as.numeric(ozone[,1])
aX = cbind(rep(1,nrow(ozone)),as.matrix(ozone[,-1])) #intercept

#estimated coefficients
betahat = solve(t(aX)%*%aX)%*%t(aX)%*%Y

#fitted data
Yhat = aX%*%betahat

#compare to lm()
cbind(betahat,t(t(fit$coefficients)))
plot(predict(fit),Yhat)

#empirically check training error when p>n
n = nrow(ozone)
n
xx = matrix(rnorm(n^2),n,n)
nX = cbind(aX,xx)
dim(nX)

#estimated coefficients
solve(t(nX)%*%nX)%*%t(nX)%*%Y
require(MASS)
nbetahat = ginv(t(nX)%*%nX)%*%t(nX)%*%Y

#fitted data
nYhat = nX%*%nbetahat
sum((Y - nYhat)^2)


