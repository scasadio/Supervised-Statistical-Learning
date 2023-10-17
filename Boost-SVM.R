## Boosting ####
library(gbm)
load("C:/Users/Utente/OneDrive/Desktop/LM/SUPERVISED/SAheart .RData")
n<-nrow(SAheart)
x<-SAheart[,-10] 
y<-SAheart[,10]

heart<-data.frame(chd=y,x)
set.seed(1234)
train<-sample(1:n,ceiling(n/2))
heart.test<-SAheart[-train,]
heart.train<-SAheart[train,]

out.boost<-gbm(chd~., data=heart[train,],
               distribution="bernoulli",
               n.trees=100, interaction.depth = 4,
               bag.fraction = 1)
out.boost 
summary(out.boost) 
K=5
set.seed(1234)
folds<-sample(1:K,length(train),replace=T)
table(folds)
B<-c(25,50,100,150) #no.trees options
err.cv<-matrix(NA,K,length(B))

for (k in 1:K){
  x.test<-heart.train[folds==k,]
  x.train<-heart.train[folds!=k,]
  
  for (b in 1:length(B)){
    boost.out<-gbm(chd~.,x.train,
                   distribution="bernoulli",
                   n.trees=B[b], interaction.depth = 4,
                   bag.fraction = 1)
    p.hat<-predict(boost.out,newdata=x.test,
                   n.trees=B[b],
                   type="response")
    yhat<-ifelse(p.hat>0.5,1,0)
    err.cv[k,b]<-1-mean(yhat==x.test$chd) #1-fraction of units correctly classified
  }
}

colMeans(err.cv) #the optimal number of trees is the smallest B, so 25 trees (?)
best.B<-B[which.min(colMeans(err.cv))]

#Let's estimate the whole thing on the training
boost.heart<-gbm(chd~.,heart.train,
                 distribution = "bernoulli",
                 n.trees = best.B,
                 interaction.depth = 4,
                 bag.fraction = 1)
#prediction on the test set
p.hat.test<-predict(boost.heart,newdata = heart.test,
                    n.trees = best.B,
                    type="response")
y.hat.test<-ifelse(p.hat.test>0.5,1,0)
table(y.hat.test,heart.test$chd)
1-mean(y.hat.test==heart.test$chd)

## SVM ####

## Support Vector Classifier ####
install.packages("e1071")
library(e1071)
heart<-data.frame(chd=as.factor(y),x)
svmfit<-svm(chd~.,data=heart, kernel="linear",
            cost=10)
svmfit
summary(svmfit)
nrow(SAheart)
svmfit$index

svmfit<-svm(chd~.,data=heart, subset = train, kernel="linear",
            cost=1000)
#svmfit111<-svm(chd~.,data = heart[train,], kernel="linear",cost=1000)
svmfit

set.seed(1234)
tune_out<-tune(svm,chd~.,data=heart[train,],
               kernel="linear",
               ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
tune_out

best_model<-tune_out$best.model
yhat<-predict(best_model,heart[-train,])

table(yhat,heart$chd[-train])
1-mean(yhat==heart$chd[-train])

##support vector machines###
###Radial kernel###
svmfit<-svm(chd~.,data=heart[train,],kernel="radial",gamma=1,cost=1)
summary(svmfit)

set.seed(1234)#to tune the parameters
svm_out<-tune(svm,chd~., data=heart[train,],kernel="radial",
              ranges=list(cost=c(0.1,1,10,100),
                          gamma=c(1,2,3,4)))
summary(svm_out)
yhat.rad<-predict(svm_out$best.model,newdata=heart[-train,])
table(yhat.rad,heart$chd[-train])
1-mean(yhat.rad==heart$chd[-train])

##polynomial kernel####
set.seed(1234)
svm_out<-tune(svm,chd~.,data=heart[train,],kernel="polynomial",
              ranges = list(cost=c(0.1,1,10,100),
                            d=1:5))
summary(svm_out)
yhat.pol<-predict(svm_out$best.model,newdata=heart[-train,])
table(yhat.pol,heart$chd[-train])
1-mean(yhat.pol==heart$chd[-train])

##overall best solution###
set.seed(1234)
svm_out<-tune(svm,chd~.,data=heart[train,],
              ranges = list(cost=c(0.1,1,10),
                            kernel=c('linear','radial','polynomial')))
summary(svm_out)
yhat.overall<-predict(svm_out$best.model,newdata=heart[-train,])
table(yhat.overall,heart$chd[-train])
1-mean(yhat.overall==heart$chd[-train])
