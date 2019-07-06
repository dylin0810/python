#############
#lecture 13 - PCA

##################
install.packages("ISLR")
require("ISLR")

ncidat = t(NCI60$data)
colnames(ncidat) = NCI60$labs

dim(ncidat)
unique(colnames(ncidat))

#PCA - take SVD to get solution
#center genes, but don't scale
X = t(scale(t(ncidat),center=TRUE,scale=FALSE))
sv = svd(t(X)); # t(X) is n by p; hence right matrix is loading
U = sv$u 
V = sv$v # loading
D = sv$d
Z = t(X)%*%V;
Z - U%*%diag(D) 

#PC scatterplots
cols = as.numeric(as.factor(colnames(ncidat)))
K = 3
pclabs = c("PC1","PC2","PC3","PC4")
par(mfrow=c(1,K))
for(i in 1:K){
  j = i+1
  plot(U[,i],U[,j],type="n",xlab=pclabs[i],ylab=pclabs[j])
  text(U[,i],U[,j],colnames(X),col=cols)
}

#PC loadings - visualize data by limiting to top genes in magnitude in the PC loadings 
aa = grep("grey",colors())
bb = grep("green",colors())
cc = grep("red",colors())
gcol2 = colors()[c(aa[1:30],bb[1:20],rep(cc,2))]

j = 2
ord = order(abs(V[,j]),decreasing=TRUE)
x = as.matrix(X[ord[1:250],])
heatmap(x,col=gcol2)

#Variance Explained
varex = 0
cumvar = 0
denom = sum(D^2)
for(i in 1:64){
  varex[i] = D[i]^2/denom
  cumvar[i] = sum(D[1:i]^2)/denom
}

#screeplot
par(mfrow=c(1,2))
plot(1:64,varex,type="l",lwd=2,xlab="PC",ylab="% Variance Explained")
plot(1:64,cumvar,type="l",lwd=2,xlab="PC",ylab="Cummulative Variance Explained")


#######
#Sparse PCA
install.packages("PMA")
source("https://bioconductor.org/biocLite.R")
biocLite("impute")
library(impute)
require("PMA")

spc = SPC(t(X),sumabsv=10,K=4)

#how many genes selected?
apply(abs(spc$v)>1e-8,2,sum)

#PC scatterplots
cols = as.numeric(as.factor(colnames(ncidat)))
K = 3
pclabs = c("SPC1","SPC2","SPC3","SPC4")
par(mfrow=c(1,K))
for(i in 1:K){
  j = i+1
  plot(spc$u[,i],spc$u[,j],type="n",xlab=pclabs[i],ylab=pclabs[j])
  text(spc$u[,i],spc$u[,j],colnames(X),col=cols)
}

#SPC loadings - visualize data by limiting to gene selected by the sparse PC loadings
aa = grep("grey",colors())
bb = grep("green",colors())
cc = grep("red",colors())
gcol2 = colors()[c(aa[1:30],bb[1:20],rep(cc,2))]

j = 1
ind = which(spc$v[,j]!=0)
x = as.matrix(X[ind,])
heatmap(x,col=gcol2)

#variance explained
spc$prop.var.explained


###########################
#digits data
load("UnsupL.Rdata")


dat1 = read.table('zip.train')
dat2 = read.table('zip.test')
digits = rbind(dat1,dat2)

#pull out 3's
dat3 = digits[(digits[,1]==3),-1]

#visulaize
par(mfrow=c(3,4))
for(i in 1:12){
  imagedigit(as.numeric(dat3[i,]))
}

#PCA - take SVD to get solution
#don't center and scale to retain interpretation as images
svd3 = svd(dat3)
U = svd3$u
V = svd3$v #PC loadings
D = svd3$d
Z = as.matrix(dat3)%*%V #PCs

#PC scatterplot
par(mfrow=c(1,1))
plot(Z[,1],Z[,2],pch=16)

#PC loadings
par(mfrow=c(1,4))
for(i in 1:4){
  imagedigit(V[,i])
}

#Variance Explained
varex = 0
cumvar = 0
denom = sum(D^2)
for(i in 1:256){
  varex[i] = D[i]^2/denom
  cumvar[i] = sum(D[1:i]^2)/denom
}

#screeplot
par(mfrow=c(1,2))
plot(1:256,varex,type="l",lwd=2,xlab="PC",ylab="% Variance Explained")
plot(1:256,cumvar,type="l",lwd=2,xlab="PC",ylab="Cummulative Variance Explained")


cumvar[25] #first 25 PCs explain over 90% of variance
pdat3 = as.matrix(dat3)%*%V[,1:25] #projected data - a tenth of the original size


##########
#now all digits

svdd = svd(digits)
U = svdd$u
V = svdd$v #PC loadings
D = svdd$d
Z = as.matrix(digits)%*%V #PCs

#PC scatterplot
i = 1; j = 2;
plot(U[,i],U[,j],type="n")
text(U[,i],U[,j],rownames(digits),col=rownames(digits),cex=.7)

#PC loadings
par(mfrow=c(3,5))
for(i in 1:15){
  imagedigit(V[,i])
}

#Variance Explained
varex = 0
cumvar = 0
denom = sum(D^2)
for(i in 1:256){
  varex[i] = D[i]^2/denom
  cumvar[i] = sum(D[1:i]^2)/denom
}

#screeplot
par(mfrow=c(1,2))
plot(1:256,varex,type="l",lwd=2,xlab="PC",ylab="% Variance Explained")
plot(1:256,cumvar,type="l",lwd=2,xlab="PC",ylab="Cummulative Variance Explained")


