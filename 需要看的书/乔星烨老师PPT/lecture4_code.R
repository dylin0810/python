###########################
#lecture 4 code
#ozone data
#feature selection

#libraries required
library(MASS)
library(leaps)
library(glmnet)


#load data from the R package (if you don't have the .csv file)
library(cosso)
data(ozone)
#OR load data from .csv file
ozone = read.csv("ozone.csv")

#take a look at the data
plot(ozone,pch=16,cex=.5)


########################################
#feature selection in linear models

#####################
#Algorithmic

#best subsets selection
fitbsub = regsubsets(x=ozone[,2:9],y=ozone[,1])
summary(fitbsub)

#forward step-wise  - via BIC
fit0 = lm(ozone~1,data=ozone)
fitf = stepAIC(fit0,scope=ozone~wind+temp+invHt+press+hum+vis+milPress+invTemp,direction="forward",data=ozone,k=log(nrow(ozone)))
summary(fitf)

#backwards step-wise - via BIC
fit = lm(ozone~.,data=ozone)
fitb = stepAIC(fit,direction="backward",data=ozone,k=log(nrow(ozone)))
summary(fitb)


############################
#L1 Regularization

Y = as.numeric(ozone[,1]); Y = Y - mean(Y) #center response
X = as.matrix(ozone[,-1]); X = scale(X,center=T,scale=T) #center and scale data matrix
fit0 = lm(Y~X-1)

#fit lasso with fixed lambda
lam = 1
fitl = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=1)
cbind(fit0$coef,as.matrix(fitl$beta))


#lasso paths
fitl = glmnet(x=X,y=Y,family="gaussian",alpha=1)
plot(fitl,col=1:8)
legend(0,4,legend=names(ozone)[2:9],col=1:8,lty=rep(1,8),cex=.8)

