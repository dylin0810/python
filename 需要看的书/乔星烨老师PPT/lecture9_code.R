###########
#lecture 9 code

library(glm2)

#########################
##Log-linear - horseshoe crab data
data(crabs)
str(crabs)
fit.log.linear <- glm(Satellites~., data = crabs, family = 'poisson')
summary(fit.log.linear)


###########
#sparse logistic regression


library(glmnet)
library(ncvreg)



#reading in data
data = read.csv("spam_dat.csv",header=FALSE)
ndat = read.delim("spambase2.names",header=FALSE)

#parsing variable names
nams = NULL
for(i in 1:nrow(ndat))
{
  vec = strsplit(as.character(ndat[i,]),split="_")
  for(j in 1:length(vec[[1]]))
    {
      if(length(grep(":",vec[[1]][j]))>0)
        {
          vars = strsplit(vec[[1]][j],split=":")
          nams = c(nams,vars[[1]][1])
        }
    }
}

Y = data[,58]
n = length(Y)
sum(Y)/n
X = as.matrix(log(1 + data[,1:57]))
colnames(X) = nams
X = scale(X)/sqrt(n-1)
dat = data.frame(Y,X)

#taking a subset of data
sdat = data.frame(Y,dat$george,dat$meeting,dat$total,dat$re,dat$edu,dat$free,dat$your)
plot(sdat,pch=16,cex=.5)


#########
#fit logistic model

fits = glm(Y~.,data=sdat,family="binomial",maxit=50)
summary(fits)


#penalized logistic - coefficients
lam = .1

Xs = as.matrix(sdat[,2:8])
sfitl = glmnet(x=Xs,y=Y,family="binomial",lambda=lam,alpha=1)
sfitr = glmnet(x=Xs,y=Y,family="binomial",lambda=lam,alpha=0)
sfitel = glmnet(x=Xs,y=Y,family="binomial",lambda=lam,alpha=.5)
sfitmcp = ncvreg(Xs,Y,family="binomial",penalty="MCP",lambda=lam)
mat = cbind(fits$coefficients[-1],as.matrix(sfitl$beta),as.matrix(sfitr$beta),as.matrix(sfitel$beta),sfitmcp$beta[-1])
colnames(mat) = c("Logistic","Lasso","Ridge","EL","MCP")
mat

#penalized logistic - regularization paths
sfitl = glmnet(x=Xs,y=Y,family="binomial",alpha=1)
sfitr = glmnet(x=Xs,y=Y,family="binomial",alpha=0)
sfitel = glmnet(x=Xs,y=Y,family="binomial",alpha=.5)
sfitmcp = ncvreg(Xs,Y,family="binomial",,penalty="MCP")

par(mfrow=c(2,2))
plot(sfitl,col=1:7,main="L1")
legend(0,-100,legend=names(sdat)[2:8],col=1:7,lty=rep(1,7),cex=.75)

plot(sfitr,col=1:7,main="Ridge")
legend(0,-10,legend=names(sdat)[2:8],col=1:7,lty=rep(1,7),cex=.75)

plot(sfitel,col=1:7,main="Elastic Net")
legend(0,-100,legend=names(sdat)[2:8],col=1:7,lty=rep(1,7),cex=.75)

plot(sfitmcp,col=1:7,main="MC+",shade=F)
legend(.22,-100,legend=names(sdat)[2:8],col=1:7,lty=rep(1,7),cex=.75)


#note: this takes a while to run
#full data L1 regularizaiton paths
fit1 = glmnet(x=X,y=Y,family="binomial")
plot(fit1)
