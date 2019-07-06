########################
#lecture 10 code
#naive Bayes and LDA
install.packages(c('grDevices','geigen','ggplot2','naivebayes','Metrics'))
library(grDevices)
library(geigen)
library(ggplot2)
library(naivebayes)
library(MASS)
library(Metrics)

## import the data
dat <- read.table('zip.train')
x <- as.matrix(dat[,2:dim(dat)[2]])
n <- dim(x)[1]
p <- dim(x)[2]


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


## set up the indicator matrix 
nclass <- length(unique(dat[,1]))
Y <- matrix(0,n,nclass)
nK <- numeric(nclass)

for (i in 1:nclass){
  Y[dat[,1]==(i-1),i] <- 1
  nK[i] <- sum(Y[,i])
}

## center the columns of x
X <- scale(x,center=TRUE,scale=FALSE)

## compute the mean of each variable for each class
mvecs <- matrix(0,p,nclass)
for (i in 1:nclass){
  mvecs[,i] <- apply(X[Y[,i]==1,],2,mean)
}

## compute between-group covariance
SigB <- matrix(0,p,p)
for (i in 1:nclass){
  SigB <- SigB + nK[i]*(matrix(mvecs[,i],p,1)%*%t(matrix(mvecs[,i],p,1)))
}

## compute total covariance
SigT <- t(X)%*%X

## compute within-group covariance
SigW <- SigT-SigB

## solve the generalized eigenvalue problem 
eig <- geigen(SigB,SigW)

## sort the eigenvalues in decreasing order
D <- sort(eig$values,decreasing = TRUE, index.return = TRUE)
D.sorted <- D$x
V <- eig$vectors[,D$ix]

plot.frame <- data.frame(first.proj=(X%*%V[,1]),second.proj=(X%*%V[,2]),digits=as.factor(dat[,1]))
ggplot(plot.frame) + 
  geom_point(aes(x=first.proj,y=second.proj,colour=digits)) + 
  xlab('Data projected onto the first discriminant coordinate') + 
  ylab('Data projected onto the second discriminant coordinate')

plot.frame.2 <- data.frame(second.proj=(X%*%V[,2]),third.proj=(X%*%V[,3]),digits=as.factor(dat[,1]))
ggplot(plot.frame.2) + 
  geom_point(aes(x=second.proj,y=third.proj,colour=digits)) + 
  xlab('Data projected onto the second discriminant coordinate') + 
  ylab('Data projected onto the third discriminant coordinate')

## use Naive Bayes classifier 
NB <- naive_bayes(x=X,y=as.factor(dat[,1]))
(NB.train.error <- ce(as.factor(dat[,1]),predict(NB,X,type='class')))

## use built-in LDA function
LDA <- lda(X,as.factor(dat[,1]))
(LDA.train.error <- ce(as.factor(dat[,1]),predict(LDA,X)$class))

## use our LDA computed via generalized eigenvalue problem 
proj.X <- X%*%V[,1:7]
myLDA <- naive_bayes(x=proj.X,y=as.factor(dat[,1]))
(myLDA.train.error <- ce(as.factor(dat[,1]),predict(myLDA,proj.X,type='class')))


## import test data
tdat <- read.table('zip.test')
tx <- as.matrix(tdat[,2:dim(tdat)[2]])
tn <- dim(tx)[1]
tp <- dim(tx)[2]

tX <- scale(tx,center=TRUE,scale=FALSE)

##predict using Naive Bayes classifier
(NB.test.error <- ce(as.factor(tdat[,1]),predict(NB,tX,type='class')))

## predict using built-in LDA function
(LDA.test.error <- ce(as.factor(tdat[,1]),predict(LDA,tX)$class))

##predict using our LDA function
(myLDA.test.error <- ce(as.factor(tdat[,1]),predict(myLDA,(tX%*%V[,1:7]),type='class')))
