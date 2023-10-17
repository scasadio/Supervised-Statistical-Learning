#logistic regression ####
#install.packages('ElemStatLearn')
#library(ElemStatLearn)

#LOADING THE DATA ####
load("C:/Users/Utente/Downloads/SAheart.RData")
summary(SAheart)
#visualize data, plot with different points are coloured according to the class
pairs(SAheart[,-ncol(SAheart)],
      col=SAheart[,ncol(SAheart)]+1,lwd=1.5)    #-ncol remove the last column
#bivariate plots four couples of predictors
#colours of points reflect where the patients are classified in 0 or 1

#LOGISTIC REGRESSION ####
## Full logistic model ####
out.log<-glm(chd~.,data=SAheart, family = "binomial") #if i want that all other features are included in the model i use .
summary(out.log)                                      #binomial becouse the response is a dummy 0/1
#std.error corresponding pvalues show that tobacco i signinficantly different from zero as ldl, famhist,typea, age
#all the other have a nonsignificant coefficients

###reduced logistic model####
out.log.vs<-glm(chd~tobacco+ldl+famhist+typea+age,data=SAheart, family = "binomial")  #model with only the significant vars
summary(out.log.vs)
#all the coefficients remain significant 
#if you compare the AIC theu are 492>487,69
#residual deviance is a little it increase cause of the reduction of the parameters but the aic it is reduced
#values of coefficients are a little bit changed

#CV ERROR ESTIMATE####
#prepare a cross validation 
x<-subset(SAheart, 
          select = c("chd","tobacco","ldl","famhist","typea","age"))
#load the misc function to compute the error
misc<-function(yhat,y){
  if (length(table(yhat))!=length(table(y)))
    stop("The levels of the two vectors do not match")
  1-sum(diag(table(yhat,y)))/length(y)
}

#generate for each observation in our data, a lable that goes from 1 to k, and so that we would have the same units in each folds
#let's go to the creation of the folds
k<-5
n<-nrow(SAheart)
set.seed(1234)
folds<-sample(1:k,n, replace = TRUE) #reintroduction of the units in each fold is provided
table(folds)
err.cv<-NULL

yhat<-rep(NA,n) #vector full of NA

for(i in 1:k){
  x.train<-x[folds!=i,]  #!= means different, all the elements of folds are different from the first
  x.test<-x[folds==i,]
  y.test<-x$chd[folds==i]
  
  out.cv<-glm(chd~.,data=x.train, family="binomial")
  p.hat<-predict(out.cv, newdata=x.test, type="response")
  y.hat<-ifelse(p.hat>0.5, 1,0)
  
  err.cv[i]<-misc(y.hat, y.test)  #y.hat predicted response, y.test true response
  yhat[folds==i]<-y.hat
}
err.cv #misclassification error rate for each folds

#test error estimate via cross validation
mean(ere.cv)
table(yhat,x$chd)
#more units in 1 are wrong classfied

##NAIVE BAYES CLASSIFIER####
#we assume that within each class units are independent
install.packages("klaR")
library(klaR)  #contains the function NaiveBayes

err.nb.g<-err.nb.k<-NULL
yhat.nb.g<-yhat.nb.k<-rep(NA,n)


for(i in 1:k){
  x.train<-x[folds!=i,-1]  #!= means different, all the elements of folds are different from the first
  x.test<-x[folds==i,-1]
  y.test<-x$chd[folds==i]
  y.train<-x$chd[folds!=i]
  
  #Naive Bayes with gaussian density estimate
  out.nb.g<-NaiveBayes(x=x.train, grouping = as.factor(y.train),
                     usekernel = FALSE)
  y.hat.g<-predict(out.nb.g,newdata=x.test)$class  #we want only the element class
  yhat.nb.g[folds==i]<-y.hat.g
  
  #Naive Bayes with kernel density estimate
  out.nb.k<-NaiveBayes(x=x.train, grouping = as.factor(y.train),
                       usekernel = TRUE)
  y.hat.k<-predict(out.nb.k,newdata=x.test)$class 
  yhat.nb.k[folds==i]<-y.hat.k
  
  
  err.nb.g[i]<-misc(y.hat.g,y.test)
  err.nb.k[i]<-misc(y.hat.k,y.test)
}

err.nb.g  
err.nb.k
#in some folds kernel perform better (first and second?)

#test errore estimate of the naive bayaes
mean(err.nb.g)
mean(err.nb.k)

##k-Nearest Neighbors Classifier####
install.packages('class')
library(class)

x<-SAheart[,-c(5,10)] #famhist, chd, removing lables and nominal variable
y<-SAheart[,10]

#standardize the training and after the test
set.seed(1234)
index<-sample(1:n,ceiling(n/2),replace=F)
train<-x[index,]
test<-x[-index,]
train.y<-y[index]
test.y<-y[-index]

train.std<-scale(train,T,T)  #1T dividing for standard deviation, 2T centrer into the mean
ntr<-nrow(train.std)

set.seed(1234)
folds<-sample(1:K,ntr, replace=T)
table(folds)

#we have to store a vector of neighbors we want to use and compare, and the errors for each of them
k<-c(1,3,5,11,25,45,105)
err.cv<-matrix(NA,K, length(k),dimnames = list(NULL,paste0("k=",k)))

for(i in 1:K){   #k=number of neighbors, K=number of folds
  x.train<-train.std[folds!=i,]
  y.train<-train.y[folds!=i]
  x.test<-train.std[folds==i,]
  y.test<-test.y[folds==i]
  
  for(j in 1:length(k)){
    yhat<-knn(train=x.train,test=x.test,cl=y.train,k=k[j])
    err.cv[i,j]<-misc(yhat,y.test)
  }
}

err.cv
#error for each folds for each neighbors

# in order tho choose the best k, let's compute the cross validation error
apply(err.cv,2,mean) #2 is for the column, 1 is for the rows
which.min(apply(err.cv,2,mean))

### use the validation set to estimate the test error####
mean_x<-colMeans(train)
sd_x<-apply(train,2,sd)

#standardize the test set according to the mean and sd of the training
test.std<-test
for (j in 1:ncol(test)) {
  test.std[,j]<-(test[,j]-mean_x[j])/sd_x[j]
}

#prediction
y.hat<-knn(train=train.std,test=test.std,cl=train.y, k=45)
table(y.hat,test.y)
misc(y.hat,test.y) #error of misclassification