
load("C:/Users/Utente/OneDrive/Desktop/LM/SUPERVISED/SAheart .RData")

##LAB 2 ###
#1.1 divide the dataset into training and validation sets. 
  #perform linear discriminant analysis to predict variabile chd and compute the test error estimate on the validation set

misc<-function(yhat,y){
  if (length(table(yhat))!=length(table(y)))
    stop("The levels of the two vectors do not match")
  1-sum(diag(table(yhat,y)))/length(y)
}
n<-nrow(SAheart)

set.seed(1234)
index<-sample(1:n,ceiling(n/2), replace= F)

library(MASS)

#train = SAheart[index,]
#test = SAheart[-index,]

out.lda<-lda(chd~.,data= SAheart[index,])
cl.pred<-predict(out.lda, newdata = SAheart[-index,])$class

#y.test<-SAheart[-index,]$chd
table(cl.pred, SAheart[-index,]$chd)
misc(cl.pred,SAheart[-index,]$chd)  #0.2640693

#1.2 how does it compare with logistic regression
out.log<-glm(chd~., data = SAheart[index,], family = "binomial")
summary(out.log) #AIC = 250.23

p.hat<-predict(out.log, newdata= SAheart[-index,], type ="response")
y.hat<-ifelse(p.hat>0.5,1,0)
misc(y.hat, SAheart[-index,]$chd)  #0.2683983

#Linear discriminant analysis and Logistic regression are almost identical
load("C:/Users/Utente/OneDrive/Desktop/LM/SUPERVISED/prostate.RData")

#2.1 perform model selection via best subset, forward selection and backward elimination, and hybrid approach
x<-prostate[,-ncol(prostate)]
p<-ncol(x)-1

library(leaps)

regfit.full<-regsubsets(lpsa~., x)
reg.summary<-summary(regfit.full)
names(reg.summary)

reg.summary$rsq
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

par(mfrow=c(2,2))
plot(reg.summary$adjr2, xlab="Number of variables", ylab = "adjr2", type= "l")

coef(regfit.full,7)

regfit.fwd<-regsubsets(lpsa~., x, method = "forward")
#for k=0,1,..,p-1 consider all p-k models that augment the predictors in Mk with one additional predictor
regfit.bwd<-regsubsets(lpsa~., x, method = "backward")
#for k=p,p-1,..,1 consider all k model M that consider all but one of the predictors in Mk.
regfit.hyb<-regsubsets(lpsa~., x, method = "seqrep")

#2.2 choose the best model via validation set approach and cross validation. estimate the test error
set.seed(1234)
train<-sample(c(TRUE,FALSE),nrow(x), rep=TRUE)
test<-(!train)

regfit.best<-regsubsets(lpsa~.,data = x[train,])
test.mat<-model.matrix(lpsa~.,data=x[test,])#model.matrix used for building a matrix X from data

val.error<-rep(NA,p)
for (i in 1:p) {
  coefi<-coef(regfit.best, id =i)
  pred<-test.mat[,names(coefi)]%*%coefi
  val.error[i]<-mean((x$lpsa[test]-pred)^2)
}

which.min(val.error) #the best model is the one that contains 5 variables
coef(regfit.best,which.min(val.error))

#but
predict.regsubset<-function(object, newdata, id,...){
  form<-as.formula(object$call [2])
  mat<-model.matrix(form, newdata)
  coefi<-coef(object,id=id)
  xvars<-names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best<-regsubsets(lpsa~.,data = x)  #we use the complete set of data in order to obtain more accurate coefficient estimates
coef(regfit.best,5)

#cross-validation approach
K<-5
set.seed(1234)
folds<-sample(1:K, nrow(x),replace=T)
cv.errors<-matrix(NA,K,p, dimnames = list(NULL, paste(1:p)))

for (j in 1:K) {
  best.fit<-regsubsets(lpsa~.,x[folds!=j,])
  for (i in 1:p){
    pred<-predict(best.fit,x[folds==j,], id=i)    #ERROR!!
    cv.errors[j,i]<-mean((x$lpsa[folds==j]-pred)^2)
  }
} 
