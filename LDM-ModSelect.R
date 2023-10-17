# Lab II May 6th #####

# Classification ####
## Linear Discriminant Analysis ####
load("C:/Users/Utente/OneDrive/Desktop/LM/SUPERVISED/SAheart .RData")
#source("misc.R")
misc<-function(yhat,y){
  if (length(table(yhat))!=length(table(y)))
    stop("The levels of the two vectors do not match")
  1-sum(diag(table(yhat,y)))/length(y)
}
  
### Training + Validation sets
n<-nrow(SAheart)
set.seed(1234)
index<-sample(1:n,ceiling(n/2),replace=F)

library(MASS)
out.lda<-lda(chd~.,data=SAheart[index,])
out.lda
cl.pred<-predict(out.lda,newdata = SAheart[-index,])$class


table(cl.pred,SAheart$chd[-index])
misc(cl.pred,SAheart$chd[-index])

## Logistic Regression ####
out.log<-glm(chd~.,data=SAheart[index,],family="binomial")
p.hat.lr<-predict(out.log,newdata=SAheart[-index,],type="response")
y.hat.lr<-ifelse(p.hat.lr>0.5,1,0)

table(y.hat,y.hat.lr)

misc(y.hat.lr,SAheart$chd[-index])


# Linear Model Selection ####
load("C:/Users/Utente/OneDrive/Desktop/LM/SUPERVISED/prostate.RData")

head(prostate)

x<-prostate[,-ncol(prostate)]
p<-ncol(x)-1

summary(x)

install.packages('leaps')
library(leaps)


## Best subset selection ####
regfit.full<-regsubsets(lpsa~.,x)
summary(regfit.full)

reg.summary<-summary(regfit.full)
names(reg.summary)

reg.summary$rsq
which.max(reg.summary$adjr2)
which.min(reg.summary$bic)
which.min(reg.summary$cp)

par(mfrow=c(2,2)) # divides the plot window into 4 regions
plot(reg.summary$rss,xlab="No. of variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="No. of variables",ylab="Adj. R2",type="l")
plot(reg.summary$bic,xlab="No. of variables",ylab="BIC",type="l")
plot(reg.summary$cp,xlab="No. of variables",ylab="C_p",type="l")

coef(regfit.full,7)

## Forward selection ####
regfit.fwd<-regsubsets(lpsa~.,x,method="forward")
summary(regfit.fwd)

## Backward elimination ####
regfit.bwd<-regsubsets(lpsa~.,x,method="backward")
summary(regfit.bwd)

## Hybrid version ####
regfit.seqrep<-regsubsets(lpsa~.,x,method="seqrep")
summary(regfit.seqrep)

## Validation set approach ####
set.seed(1234)
train<-sample(c(TRUE,FALSE),nrow(x),replace=T)
test<-!train
table(train)
regfit.best<-regsubsets(lpsa~.,data=x[train,])

test.mat<-model.matrix(lpsa~.,data=x[test,])

mse.valid<-rep(NA,8)
for (i in 1:8){
  coefi<-coef(regfit.best,id=i)
  yhat<-test.mat[,names(coefi)]%*%coefi
  mse.valid[i]<-mean((x$lpsa[test]-yhat)^2)
}
mse.valid
which.min(mse.valid)
coef(regfit.best,which.min(mse.valid))

## Cross-Validation ####
predict.regsubsets<-function(object,newdata,id,...){
  form<-as.formula(object$call[[2]]) # we specify the formula of the lm
  mat<-model.matrix(form,newdata)
  coefi<-coef(object,id=id)
  xvars<-names(coefi)
  yhat<-c(mat[,xvars]%*%coefi)
  return(yhat)
}

predict(regfit.best,x[test,],id=5)

regfit.best<-regsubsets(lpsa~.,data=x)
coef(regfit.best,5)

k<-5
set.seed(1234)
folds<-sample(1:k,nrow(x),replace = T)
cv.error<-matrix(NA,k,p,dimnames = list(NULL,paste(1:p)))

#for (i in 1:k){
  #train.x<-x[folds!=i,]
  #test.x<-x[folds==i,]
  #best.fit<-regsubsets(lpsa~.,train.x)
  #for (j in 1:8){
    #pred<-predict(best.fit,test.x,id=j)
    #cv.error[i,j]<-mean((test.x$lpsa-pred)^2)
  #}
#}
for (j in 1:k) {
  best.fit<-regsubsets(lpsa~.,data=x[folds!=j,])
  for (i in 1:p) {
    pred<-predict(best.fit,x[folds==j,],id=i)
    cv.error[j,i]<-mean((x$lpsa[folds==j]-pred)^2)
  }
}


apply(cv.error,2,mean)
which.min(apply(cv.error,2,mean))

best.model<-regsubsets(lpsa~.,x)
best.bhat3<-coef(best.model,id=3)
best.bhat3
