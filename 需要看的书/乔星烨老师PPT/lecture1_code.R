#####################
## Lecture 1 code
## K Nearest Neighbors Classification

library(class)
library(ggplot2)

## Initialize parameters
n = 300
sig = 1.5
mu1 = c(2,7); mu2 = c(7,2); mu3 = c(7,7)

## Simulate training data matrix, test data matrix, and class label
x1 = t(matrix(mu1,2,n/3)) + matrix(rnorm(n)*sig,n/3,2)
x2 = t(matrix(mu2,2,n/3)) + matrix(rnorm(n)*sig,n/3,2)
x3 = t(matrix(mu3,2,n/3)) + matrix(rnorm(n)*sig,n/3,2)
Xtrain = rbind(x1,x2,x3)
x1 = t(matrix(mu1,2,n/3)) + matrix(rnorm(n)*sig,n/3,2)
x2 = t(matrix(mu2,2,n/3)) + matrix(rnorm(n)*sig,n/3,2)
x3 = t(matrix(mu3,2,n/3)) + matrix(rnorm(n)*sig,n/3,2)
Xtest = rbind(x1,x2,x3)
Y = c(rep(1,n/3),rep(2,n/3),rep(3,n/3))
y = factor(Y)

train.df <- data.frame(var1=Xtrain[,1], var2=Xtrain[,2], label = y)

## Set color map
color_map_three <- c('#000000', '#E74C3C', '#2ECC71')
## Plot training data in 2-dimensional feature space and color by true class label
ggplot(train.df, aes(x=var1, y=var2, colour=label)) + 
  geom_point(size=1.5) + 
  labs(title='Training Data') + 
  xlab('Feature 1') + 
  ylab('Feature 2') +
  scale_colour_manual('True Label', values = color_map_three) + 
  theme(plot.title = element_text(hjust=0.5, size = 20, face = 'bold')) + 
  theme(axis.title = element_text(size = 16, face = 'bold')) + 
  theme(axis.text.y = element_text(size = 12, face = 'bold')) +
  theme(axis.text.x = element_text(size = 12, face = 'bold')) +
  theme(legend.title = element_text(size = 16, face = 'bold'),
        legend.text = element_text(size = 14, face = 'bold'))


## Set parameter for the KNN classifier
k=5
## Train the KNN classifier and get predicted class labels for the training set
predTr = knn(test=Xtrain,train=Xtrain,cl=y,k=k)
train.df$pred.label <- predTr

## Plot the training data with both true class label and predicted class label
ggplot(train.df) + 
  geom_point(aes(x=var1, y=var2, colour=label)) + 
  geom_point(aes(x=var1, y=var2, fill=pred.label, colour=pred.label), shape=3, size=3) + 
  labs(title='KNN Classifier on Training Data') + 
  xlab('Feature 1') + 
  ylab('Feature 2') +
  scale_colour_manual('True Label', values = color_map_three) + 
  scale_fill_manual('Predicted Label', values = color_map_three, guide=guide_legend(override.aes=list(colour=color_map_three))) + 
  theme(plot.title = element_text(hjust=0.5, size = 20, face = 'bold')) + 
  theme(axis.title = element_text(size = 16, face = 'bold')) + 
  theme(axis.text.y = element_text(size = 12, face = 'bold')) +
  theme(axis.text.x = element_text(size = 12, face = 'bold')) +
  theme(legend.title = element_text(size = 16, face = 'bold'),
        legend.text = element_text(size = 14, face = 'bold'))

## Compute training error 
errTr = sum(predTr!=y)/n
errTr

## Train the KNN classifier and get predicted class labels for the test set
predTest = knn(test=Xtest,train=Xtrain,cl=y,k=k)
test.df <- data.frame(var1=Xtest[,1], var2=Xtest[,2], label = y, pred.label = predTest)

## Plot the test data with both true class label and predicted class label
ggplot(test.df) + 
  geom_point(aes(x=var1, y=var2, colour=label)) + 
  geom_point(aes(x=var1, y=var2, fill=pred.label, colour=pred.label), shape=3, size=3) + 
  labs(title='KNN Classifier on Test Data') + 
  xlab('Feature 1') + 
  ylab('Feature 2') +
  scale_colour_manual('True Label', values = color_map_three) + 
  scale_fill_manual('Predicted Label', values = color_map_three, guide=guide_legend(override.aes=list(colour=color_map_three))) + 
  theme(plot.title = element_text(hjust=0.5, size = 20, face = 'bold')) + 
  theme(axis.title = element_text(size = 16, face = 'bold')) + 
  theme(axis.text.y = element_text(size = 12, face = 'bold')) +
  theme(axis.text.x = element_text(size = 12, face = 'bold')) +
  theme(legend.title = element_text(size = 16, face = 'bold'),
        legend.text = element_text(size = 14, face = 'bold'))

## Compute test error
errTest = sum(predTest!=y)/n
errTest


#############
## Model complexity

## Set random seed for reproducibile results
set.seed(1111)
n = 300
sig = 1.5
mu1 = c(2,7); mu2 = c(7,2); mu3 = c(7,7)
x1 = t(matrix(mu1,2,n/3)) + matrix(rnorm(n)*sig,n/3,2)
x2 = t(matrix(mu2,2,n/3)) + matrix(rnorm(n)*sig,n/3,2)
x3 = t(matrix(mu3,2,n/3)) + matrix(rnorm(n)*sig,n/3,2)
Xtrain = rbind(x1,x2,x3)
x1 = t(matrix(mu1,2,n/3)) + matrix(rnorm(n)*sig,n/3,2)
x2 = t(matrix(mu2,2,n/3)) + matrix(rnorm(n)*sig,n/3,2)
x3 = t(matrix(mu3,2,n/3)) + matrix(rnorm(n)*sig,n/3,2)
Xtest = rbind(x1,x2,x3)
Y = c(rep(1,n/3),rep(2,n/3),rep(3,n/3))
y = factor(Y)

## Train KNN classifier with k=1,...,50 and record the training and test error for each k
ks = c(1:50)
errTr = 0
errTs = 0
for(i in 1:length(ks)){
  predTest = knn(test=Xtest,train=Xtrain,cl=y,k=ks[i])
  errTs[i] = sum(y!=predTest)/length(predTest)
  predTr = knn(test=Xtrain,train=Xtrain,cl=y,k=ks[i])
  errTr[i] = sum(y!=predTr)/length(predTr)
}

## Plot training error and test error with respect to k
plot(ks,errTs,col=2,xlim=rev(range(ks)),xlab="K / Model Complexity",ylab="Error")
points(ks[50:1],errTr[50:1],col=1)
lines(lowess(x=ks[50:1],y=errTs[50:1],f=.25),col=2)
lines(lowess(x=ks[50:1],y=errTr[50:1],f=.25),col=1)
legend(50,.15,legend=c("Training Error","Test Error"),col=c(1,2),lty=c(1,1))

#######################
## Curse of dimensionality

## Simulate training and test data with n=300 and p=500 (p>n)
n = 300
p0 = 5000
sig = 1.5
mu1 = c(2,7); mu2 = c(7,2); mu3 = c(7,7)
x1 = t(matrix(mu1,2,n/3)) + matrix(rnorm(n)*sig,n/3,2)
x2 = t(matrix(mu2,2,n/3)) + matrix(rnorm(n)*sig,n/3,2)
x3 = t(matrix(mu3,2,n/3)) + matrix(rnorm(n)*sig,n/3,2)
Xtrain = cbind(rbind(x1,x2,x3),matrix(rnorm(n*p0),n,p0))
x1 = t(matrix(mu1,2,n/3)) + matrix(rnorm(n)*sig,n/3,2)
x2 = t(matrix(mu2,2,n/3)) + matrix(rnorm(n)*sig,n/3,2)
x3 = t(matrix(mu3,2,n/3)) + matrix(rnorm(n)*sig,n/3,2)
Xtest = cbind(rbind(x1,x2,x3),matrix(rnorm(n*p0),n,p0))
Y = c(rep(1,n/3),rep(2,n/3),rep(3,n/3))
y = factor(Y)

train.df.hd <- data.frame(var1=Xtrain[,1], var2=Xtrain[,2], label = y)


## Set color map
color_map_three <- c('#000000', '#E74C3C', '#2ECC71')
## Plot training data in 2-dimensional feature space and color by class label
ggplot(train.df.hd, aes(x=var1, y=var2, colour=label)) + 
  geom_point(size=1.5) + 
  labs(title='Training Data in 2-d Space (p>n)') + 
  xlab('Feature 1') + 
  ylab('Feature 2') +
  scale_colour_manual('True Label', values = color_map_three) + 
  theme(plot.title = element_text(hjust=0.5, size = 20, face = 'bold')) + 
  theme(axis.title = element_text(size = 16, face = 'bold')) + 
  theme(axis.text.y = element_text(size = 12, face = 'bold')) +
  theme(axis.text.x = element_text(size = 12, face = 'bold')) +
  theme(legend.title = element_text(size = 16, face = 'bold'),
        legend.text = element_text(size = 14, face = 'bold'))

## Set parameter for KNN classifier
k=15
## Train the KNN classifier and get predicted class labels for the test set
predTest = knn(test=Xtest,train=Xtrain,cl=y,k=k)
test.df.hd <- data.frame(var1=Xtest[,1], var2=Xtest[,2], label = y, pred.label = predTest)

## Plot the test data in 2-d feature space with both true class label and predicted class label
ggplot(test.df.hd) + 
  geom_point(aes(x=var1, y=var2, colour=label)) + 
  geom_point(aes(x=var1, y=var2, fill=pred.label, colour=pred.label), shape=3, size=3) + 
  labs(title='KNN Classifier on Test Data (p>n)') + 
  xlab('Feature 1') + 
  ylab('Feature 2') +
  scale_colour_manual('True Label', values = color_map_three) + 
  scale_fill_manual('Predicted Label', values = color_map_three, guide=guide_legend(override.aes=list(colour=color_map_three))) + 
  theme(plot.title = element_text(hjust=0.5, size = 20, face = 'bold')) + 
  theme(axis.title = element_text(size = 16, face = 'bold')) + 
  theme(axis.text.y = element_text(size = 12, face = 'bold')) +
  theme(axis.text.x = element_text(size = 12, face = 'bold')) +
  theme(legend.title = element_text(size = 16, face = 'bold'),
        legend.text = element_text(size = 14, face = 'bold'))

## Compute test error
errTest = sum(predTest!=y)/n
errTest



