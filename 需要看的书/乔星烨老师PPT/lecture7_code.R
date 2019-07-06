#################
#lecture 7 - model validation

library(glmnet)

#load data from the R package (if you don't have the .csv file)
library(cosso)
data(ozone)
#OR load data from .csv file
ozone = read.csv("ozone.csv")

Y = as.numeric(ozone[,1]); Y = Y - mean(Y)
X = as.matrix(ozone[,-1]); X = scale(X,center=T,scale=T)

##############
#bootstrapping - sample observations with replacement
Xsub = X[1:10,1:3]
Xsub
bootsample = sample(1:10,10,replace=TRUE)
bootsample
Xboot = Xsub[bootsample,1:3]
Xboot

#stability selection
cvfit = cv.glmnet(X,Y)
B = 200
Smat = matrix(0,ncol(X),B)
for(i in 1:B){
  sam = sample(1:nrow(X),nrow(X),replace=TRUE)
  fit = glmnet(x=X[sam,],y=Y[sam],family="gaussian",lambda=cvfit$lambda.1se,alpha=1)
  Smat[,i] = sign(abs(as.matrix(fit$beta)))
}
Stabscores = t(t(apply(Smat,1,mean)))
rownames(Stabscores) = colnames(X)
Stabscores

###############
#inference - data splitting
sam = sample(1:nrow(X),nrow(X),replace=FALSE)
split = floor(nrow(X)*.6)
Xtr = X[sam[1:split],]; Ytr = Y[sam[1:split]]
Xts = X[sam[(split+1):nrow(X)],]; Yts = Y[sam[(split+1):nrow(X)]]

#select on training
cvfit = cv.glmnet(Xtr,Ytr)
fit = glmnet(x=Xtr,y=Ytr,family="gaussian",lambda=cvfit$lambda.1se,alpha=1)
ind = which(fit$beta!=0)

#inference on test
fit0 = lm(Yts~Xts[,ind])
summary(fit0)







