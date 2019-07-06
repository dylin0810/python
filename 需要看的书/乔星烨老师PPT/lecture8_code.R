###########
#lecture 8 code

#########################################
#logistic regression - simulated example
#note - change to coefficients to assess the fit
n = 250; p = 1;
x = matrix(rnorm(n*p),n,p)
beta0 = 1
beta = 2
eps = matrix(rnorm(n),n,1)
probs = exp(beta0 + x*beta + eps)/(1 + exp(beta0 + x*beta + eps))
Y = as.numeric(probs>.5)

#fitting logistic
fit = glm(Y ~ x,family="binomial")
summary(fit)
plot(x,Y)
xs = seq(min(x),max(x),l=1000)
pihat = exp(fit$coefficients[1] + xs*fit$coefficients[2])/(1 + exp(fit$coefficients[1] + xs*fit$coefficients[2]))
lines(xs,pihat)
lines(c(min(x),max(x)),c(.5,.5),lty=2)



###############
#looking at spam data

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

#individual variable total
fit = glm(Y~dat.total,data=sdat,family="binomial")
summary(fit)
plot(sdat$dat.total,sdat$Y)
xs = seq(min(sdat$dat.total),max(sdat$dat.total),l=1000)
pihat = exp(fit$coefficients[1] + xs*fit$coefficients[2])/(1 + exp(fit$coefficients[1] + xs*fit$coefficients[2]))
lines(xs,pihat)
lines(c(min(xs),max(xs)),c(.5,.5),lty=2)


#individual variable george
fit = glm(Y~dat.george,data=sdat,family="binomial")
summary(fit)
plot(sdat$dat.george,sdat$Y)
xs = seq(min(sdat$dat.george),max(sdat$dat.george),l=1000)
pihat = exp(fit$coefficients[1] + xs*fit$coefficients[2])/(1 + exp(fit$coefficients[1] + xs*fit$coefficients[2]))
lines(xs,pihat)
lines(c(min(xs),max(xs)),c(.5,.5),lty=2)


#individual variable free
fit = glm(Y~dat.free,data=sdat,family="binomial")
summary(fit)
plot(sdat$dat.free,sdat$Y)
xs = seq(min(sdat$dat.free),max(sdat$dat.free),l=1000)
pihat = exp(fit$coefficients[1] + xs*fit$coefficients[2])/(1 + exp(fit$coefficients[1] + xs*fit$coefficients[2]))
lines(xs,pihat)
lines(c(min(xs),max(xs)),c(.5,.5),lty=2)



