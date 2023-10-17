##str(prostate)  #quantitave response =regression problem
str(SAheart) #binary outcomes =classification problem

##load("C:/Users/Utente/OneDrive/Desktop/LM/SUPERVISED/prostate.RData")
load("C:/Users/Utente/OneDrive/Desktop/LM/SUPERVISED/SAheart .RData")

## LAB1 ###
#1.1 estimate a full logistic regression model with all the predictors
p<-ncol(SAheart)-1
#y<-SAheart$chd

out.log<-glm(chd~.,data=SAheart, family = "binomial")
  #family = "binomial" means that the response is a binary outcome

#1.2 estimate a new logistic multiple regresson model with all the relevant predictors of point 1.
#compare the coefficient estimates of the two models
summary(out.log)
  #relevant regressors are tobacco, ldl, famhist, typea, age
out.log2<-glm(chd~tobacco+ldl+famhist+typea+age, data= SAheart, family = "binomial")
summary(out.log2)
  #the new model is preferable in terms of AIC value, beacause it is less than inthe full model
  #the residual deviance is increased a little bit beacause of the reduction of the predictors as we expected

#1.3 estimate the test error of last model via 5fold cross validation
misc<-function(yhat,y){
  if (length(table(yhat))!=length(table(y)))
    stop("The levels of the two vectors do not match")
  1-sum(diag(table(yhat,y)))/length(y)
}

K<-5
n<-nrow(SAheart)
set.seed(1234)
folds<-sample(1:K,n,replace=T)
x<-subset(SAheart, select = c("chd", "tobacco", "ldl", "famhist", "typea","age"))
yy.hat<-rep(NA,n)
err.cv<-NULL
for (i in 1:K){
  x.test<-x[folds==i,]
  x.train<-x[folds!=i,]
  y.test<-x[folds==i,1]
  
  out.log.cv<-glm(chd~.,data = x.train, family = "binomial")
  p.hat<-predict(out.log.cv, newdata=x.test, type= "response")
  y.hat<-ifelse(p.hat>0.5,1,0)
  err.cv[i]<-misc(y.hat,y.test)
  
  yy.hat[folds==i]<-y.hat
}
err.cv
mean(err.cv)
misc(yy.hat,SAheart$chd)

#2 use 5 fold cross validation to estimate the test error of the naive bayes classifier
#with density estimated with both Gaussian and non parametric Kernel density
install.packages("klaR")
library(klaR)

n<-nrow(SAheart)
x<-SAheart[,-c(5,10)] #i need to remove the response variable and the categorical variable famhist
y<-SAheart[,10]

#with naive bayes classifier
K=5
set.seed(1234)
folds<-sample(1:K,n,replace=T)
err.cv.k<-err.cv.n<-yhat.k<-yhat.n<-NULL

for (i in 1:K){
  x.test<-x[folds==i,]
  x.train<-x[folds!=i,]
  y.train<-y[folds!=i]
  y.test<-y[folds==i]
  
  out.bay.k<-NaiveBayes(x=x.train, grouping = as.factor(y.train), usekernel = TRUE)
  pred.cl.k<-predict(out.bay.k, newdata=x.test)$class
  yhat.k[folds==i]<-pred.cl.k
  
  out.bay.n<-NaiveBayes(x=x.train, grouping = as.factor(y.train), usekernel = FALSE)
  pred.cl.n<-predict(out.bay.n, newdata=x.test)$class
  yhat.n[folds==i]<-pred.cl.n
  
  err.cv.k[i]<-misc(pred.cl.k,y.test)
  err.cv.n[i]<-misc(pred.cl.n,y.test)
}
mean(err.cv.k)
mean(err.cv.n)
table(yhat.k,SAheart$chd)
table(yhat.n,SAheart$chd)

#3.1 divide the dataset into training and validation set
index<-sample(1:n,ceiling(n/2), replace=F)

#3.2 use the 5fold cross validation to choose the number of neighbors among (1,3,5,11,15,25,45,105)
train<-x[index,]  #x<-SAheart[,-c(5,10)]
train_y<-y[index]
#test_y<-y[-index]
train_std<-scale(train,T,T)
ntrain<-nrow(train_std)

library(class)
K<-5
set.seed(1234)
folds<-sample(1:K,ntrain,replace=T)
k<-c(1,3,5,11,15,25,45,105)
err.cv<-matrix(NA,K, length(k), dimnames = list(NULL,paste("K=",k)))

  #i want to identify the number of neighbors in the training data that are closely to x0
for (i in 1:K) {  #K =number of folds
  x.train<-train_std[folds!=i,]
  x.test<-train_std[folds==i,]
  y.train<-train_y[folds!=i]
  y.test<-train_y[folds==i]
  
  for (j in 1:length(k)) {  #k = number of neighbors
    y.hat<-knn(train=x.train,test = x.test,cl=y.train,k=k[j])
    err.cv[i,j]<-misc(y.hat, y.test)
  }
}

apply(err.cv,2,mean)
best_k<-k[which.min(apply(err.cv,2,mean))] #45 neighbors have the minimum test error

#3.3 use the validation set to estimate the test error
#standardize the validation set
mean_x<-colMeans(train)
sd_x<-apply(train, 2, sd)

test<-x[-index,]
test_y<-y[-index]

test_std<-test

for (j in 1:ncol(test)) {
  test_std[,j]<-(test[,j]-mean_x[j])/sd_x[j]
}

#the model is fitted on the training set
#the fitted model is used to predict the response in the validation set
y.hat<-knn(train=train_std, test=test_std, cl=train_y, k=best_k)

table(y.hat,test_y)
misc(y.hat,test_y)  #0.3679654