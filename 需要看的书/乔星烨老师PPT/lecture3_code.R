###########################
#lecture 3 code
#ozone data

library(reshape2)
library(ggplot2)

#load in ozone data (taken from "cosso" R package)

#load data from the R package (if you don't have the .csv file)
library(cosso)
data(ozone)
#OR load data from .csv file
ozone = read.csv("ozone.csv")


n = nrow(ozone)

#take a look at the data
plot(ozone,pch=16,cex=.5)

#get data matrix and response vector
Y = as.numeric(ozone[,1]); Y = Y - mean(Y)
X = as.matrix(ozone[,-1]); X = scale(X,center=T,scale=T)


#ols beta estimates
betals = solve(t(X)%*%X)%*%t(X)%*%Y

#ridge regression solution
lam = 1

betar = solve(t(X)%*%X/n + diag(rep(lam,8)))%*%t(X)%*%Y/n

#compare ols estimates against ridge regression estimates 
cbind(betals,betar)


#ridge regression coefficient paths 
lambdas = exp(seq(log(.01),log(100),l=100))
betasr = matrix(0,length(lambdas),8)

#compute the ridge regression solution corresponding to each lambda value
for(i in 1:length(lambdas))
{
  betasr[i,] = solve(t(X)%*%X/n + diag(rep(lambdas[i],8)))%*%t(X)%*%Y/n
}

#plot ridge regression coefficient path using plot function
plot(range(lambdas),range(betasr),log="x",type="n",ylab="Coefficients",xlab="Log Lambda",xlim=c(max(lambdas),min(lambdas)))
for(j in 1:8)
{
  lines(lambdas,betasr[,j],type="l",col=j)
}
legend(max(lambdas),3.8,legend=names(ozone)[2:9],col=1:9,lty=rep(1,9))

#plot ridge regression coefficient path using ggplot function
#get data in a format ready for ggplot
ridge.sol <- data.frame(betasr)
colnames(ridge.sol) <- colnames(ozone[,-1])
ridge.sol$lambda <- lambdas
ridge.sol.plot.df <- melt(ridge.sol, id.vars = 'lambda')

ggplot(ridge.sol.plot.df) + 
  geom_line(aes(x=lambda, y=value, colour=variable), size = 0.9) + 
  scale_x_continuous(trans='log2') + 
  scale_color_brewer(palette = 'Set2') + 
  labs(title='Ridge Regression Coefficients Path') + 
  xlab('Log Lambda') + 
  ylab('Coefficients') +
  theme(plot.title = element_text(hjust=0.5, size = 20, face = 'bold')) + 
  theme(axis.title = element_text(size = 16, face = 'bold')) + 
  theme(axis.text.y = element_text(size = 12, face = 'bold')) +
  theme(axis.text.x = element_text(size = 12, face = 'bold')) +
  theme(legend.title = element_text(size = 16, face = 'bold'),
        legend.text = element_text(size = 14, face = 'bold'))

