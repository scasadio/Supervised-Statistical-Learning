accuracy=(TP+TN)/(P+N) #% of test set units correctly classified
error_rate= 1-accuracy
sensitivity= TP/P #true positive rate
specificity= TN/N #true negative rate
precision= TP/(TP+FP) #measure of exactness
recall=TP/(TP+FN) #completness
Fmeasure=(2*precision*recall)/(precision+recall)

#logistic regression
px<-exp(b0+b1*x1+b2*x2+b3*x3)/(1+exp(b0+b1*x1+b2*x2+b3*x3))
######## bayes theorem
clev<-  
lik_1<-dnorm(clev,mean_x1,sqrt(var_x1))  
lik_2<-dnorm(clev,mean_x2,sqrt(var_x2))
bayes<-(p1*lik_1)/((p1*lik_1)+(p2*lik_2))
########
x_train
x_test
y_test<-x_test$y
train<-sample(1:n,ceiling(n/2))
y_train<-as.factor(y[train])
y_test<-as.factor(y[train])
x_train<-as.matrix(data[train,])
x_test<-as.matrix(data[-train,])
## Logistic Regression ####
out.log<-glm(y~.,data=x_train,family="binomial")
p.hat<-predict(out.log,newdata=x_test,type="response")
y.hat<-ifelse(p.hat>0.5,1,0)
error<-misc(y.hat,y_test)

out.log<-glm(y~.,data=data,family="binomial") #binomial if y qualitative
K<-5
n<-nrow(data)
set.seed(1234)
folds<-sample(1:K,n,replace = T)
x<-subset(data, select=c("y","x1","..")) #if we retain only important regressors
yy.hat<-rep(NA,n)
err.cv<-NULL
for (i in 1:K) {
  x_test<-x[folds==i,]
  x_train<-x[folds!=i,]
  y_test<-x[folds==i,1]
  log.cv<-glm(y~.,data=x_train, family = "binomial")
  phat<-predict(log.cv, newdata=x_test, typer="response")
  y_hat<-ifelse(phat>0.5,1,0)
  err_cv[i]<-misc(y_hat, y_test)
  yy.hat[folds==i]<-y_hat
}
err.cv
mean(err.cv)
table(yy.hat,data$y) #confusion matrix #precision

## Linear Discriminant Analysis ####
library(MASS)
out.lda<-lda(y~.,data=x_train)
cl.pred<-predict(out.lda,newdata=x_test)$class
error<-misc(cl.pred, y_test)

###Naive bayes classifier ###
library(klaR)
n<-nrow(data)
datanew<-data[,-c(categorical_var,y)]
set.seed(1234)
folds<-sample(1:K,n,replace=T)
err.k<-yhat.k<-NULL #<-err.n
for(i in 1:K){
  x_test<-datanew[folds==i,]
  x_train<-datanew[folds!=i,]
  y_test<-y.vec[folds==i] #y.vec=data[,y]
  y_train<-y.vec[folds!=i]
  
  out.k<-NaiveBayes(x=xtrain, grouping=as.factor(y_train),usekernel=TRUE) #non parametric kernel #False for gaussian
  pred.k<-predict(out.k,newdata=x_test)$class
  yhat.k[folds==i]<-pred.k
  
  err.k[i]<-misc(pred.k, y_test)
}
mean(err.k)
table(yhat.k, data$y)

### KNN classifier ###
library(class)
n<-nrow(data)
datanew<-data[,-c(categorical_var,y)]
set.seed(1234)
#divide into training and validation set
index<-sample(1:n, ceiling(n/2), replace = F)
train<-datanew[index,]
train_st<-scale(train, T,T)
y.train<-y.vec[index] #data[index,]$y
ntrain<-nrow(train)

K=5
set.seed(1234)
folds<-sample(1:K,ntrain,replace=T)
k<-c(given_numbers)
err_knn<-matrix(NA, K, lenght(k),dimnames=list(NULL, paste("K=",k)))
for(i in 1:K){
  x_test<-train_st[folds==i,]
  x_train<-train_st[folds!=i,]
  y_test<-y.train[folds==i] #y.vec=data[,y]
  y_train<-y.train[folds!=i]
  
  for(j in 1:lenght(k)){
    yhat_knn<-knn(train=x_train, test=x_test, cl=y_train, k=k[j])
    err_knn[i,j]<-misc(yhat_knn, y_test)
  }
}
bestk<-k[which(apply(err_knn, 2, mean))] #best number of neighbors
#validation set to compute test error
mean_x<-colMeans(train)
sd_x<-apply(train,2,sd)
test<-datanew[-index,]
y.test<-y.vec[-index]
test_std<-test
for (j in 1:ncol(test)) {
  test_std[,j]<-(test[,j]-mean_x[j])/sd_x[j]
}
yhat_val<-knn(train=train_st, test=test_std,cl=y_train, k=bestk)
table(yhat_val, y.test)

### Best subset selection ####
library(leaps)
reg.bss<-regsubsets(y~.,data = x_train) #if 'perform model selection via..' data=data
sum_bss<-summary(reg.bss)

best_size<-which.min(sum_bss$bic) #aic..
coef(reg.bss,best_size)
yhat<-predict.regsubsets(reg.bss, x_test, id=best_size)
MSE<-mean((yhat-y_test)^2)

  #validation set approach
test.mat<-model.matrix(y~.,data = x_test) #if x_test T or F val
p<-#n. var
val.errors<-rep(NA, p)
for (i in 1:p) {
  coefi<-coef(reg.bss, id=i)
  yhat<-test.mat[,names(coefi)]%*%coefi
  val.errors[i]<-mean((y_test-yhat)^2)
}
size<-which.min(val.errors) #best model
coef(reg.bss,which.min(val.errors))
reg.bss_data<-regsubsets(y~.,data = data) 
coef(reg.bss_data,size)
   #cv approach
k=5
set.seed(1234)
folds<-sample(1:k, nrow(data), replace=TRUE)
cv.errors<-matrix(NA,k,p,dimnames=list(NULL, paste(1:p)))
for (j in 1:k){
  x_train<-data[folds!=j,]
  x_test<-data[folds==j,]
  y_test<-x_test$y
  best.fit<-regsubsets(y~., data=x_train)
  for (i in 1:p){
    pred<-predict(best.fit, x_test, id=1)
    cv.errors[j,i]<-mean((data$y[folds==j]-pred)^2)
  }
}
mean.cv.errors<-apply(cv.errors,2,mean) #which.min
coef(reg.bss_data,size_new)

### Forward selection ####
reg.fwd<-regsubsets(y~.,data = x_train, method="forward")
### Backward selection ####
reg.bwd<-regsubsets(y~.,data = x_train, method="backward")
### Hybrid selection ####
reg.hyb<-regsubsets(y~.,data = x_train, method="seqrep")

### Ridge regression ####
library(glmnet)
mat_data<-model.matrix(y~., data)[,-y]
mat_train<-model.matrix(y~., x_train)
mat_test<-model.matrix(y~., x_test)
grid<-10^seq(10,-2,length=100)
#ridge<-glmnet(mat_data,y,alpha=0,lambda = grid) perform ridge 
cv_ridge<-cv.glmnet(mat_train,y_train,alpha=0) # cross-validation ridge
best_lambda<-cv_ridge$lambda_min
ridge<-glmnet(mat_train,y_train,alpha=0,lambda = best_lambda)
which(coef(ridge)!=0)
yhat_ridge<-predict(ridge,newx=mat_test, s=best_lambda, type= "coefficients") #s=0, exact=T least squares
mse_ridge<-mean((yhat_ridge-y_test)^2)

### Lasso Regression ####
mat_train<-model.matrix(y~., x_train) #dummy var 
mat_test<-model.matrix(y~., x_test)
y_test<-X_test$y
y_train<-X_train$y
cv_lasso<-cv.glmnet(mat_train, y_train, alpha=1)
best_lambda<-cv_lasso$lambda_min
lasso<-glmnet(mat_train, y_train,alpha=1,lambda = best_lambda)
which(coef(lasso)!=0)
yhat_lasso<-predict(lasso, mat_test, s=best_lambda,type="coefficients")
mse_lasso<-mean((yhat_lasso-y_test)^2)

### PCR ####
library(pls)
data<-data[,-y]
pcr.fit<-pcr(y~.,data=data, subset=x_train,scale=T,validation="CV") #data=x_train
validationplot(pcr.fit, val.type ="MSEP", legendpos= "top")
pcr_pred<-predict(pcr.fit, x_test, ncomp=...)
error<-mean((pcr_pred-y_test)^2)

selectNcomp(pcr.fit,method="onesigma",plot=T) # Selection of no. components via heuristic
selectNcomp(pcr.fit,method="randomization",plot=T) # Selection of no. components via randomization


### Regression trees ####
library(tree)
regr_tree<-tree(y~.,x_train) #data, subset=train
summary(regr_tree)
plot(regr_tree)
text(regr_tree,pretty=0,digits=3)
yhat_tree<-predict(regr_tree, x_test)
mse_tree<-mean((yhat_tree-y_test)^2)

#
cv_tree<-cv.tree(regr_tree,K=5,FUN=prune.tree)
best_size<- cv_tree$size[which.min(cv_tree$dev)]
plot(prune_tree)
text(prune_tree, pretty=0)
prune_tree<-prune.tree(regr_tree,best=best_size)
yhat_cvtree<-predict(prune_tree,x_test)
mse_tree<-mean((yhat_cvtree-y_test)^2)

### new variable ###
x_train$new<-ifelse(y_train>threshold,0,1) #1 otherwise in text
x_test$new<-ifelse(y_test>threshold,0,1) 
ncol(x_train)
ncol(x_test)
train_new<-x_train[,-8] #prev y to remove
test_new<-x_test[,-8]
train_new$class<-as.factor(train_new$class)
test_new$class<-as.factor(test_new$class)

### Classification trees ####
#x<-data[,-y]  y<-data[,y]
clas_tree<-tree(y~.,data, subset=x_train) #data<-data(nomey=as.factor(y),x)
plot(clas_tree)
text(clas_tree,pretty=0)
tree_pred<-predict(clas_tree,x_test, type="class")
error<-misc(tree_pred,y_test)

cv_tree<-cv.tree(clas_tree,FUN=prune.misclass)
best_size<-cv_tree$size[which.min(cv_tree$dev)]
plot(prune_tree)
text(prune_tree, pretty=0)
prune_tree<-prune.misclass(clas_tree,best=best_size)
cvtree_pred<-predict(prune_tree,x_test, type="class")
error<-misc(tree_pred,y_test)

## Bagging, Random Forests and Boosting ####
data=data.frame(y=as.factor(y), data) #comprendono train e test
p=ncol(data)-1
set.seed()
### Bagging
library(randomForest)
bag<-randomForest(y~.,data=x_train,mtry=p,importance=TRUE) #data=data, subset=train
importance(bag) #average decreasing gini
varImpPlot(bag)
yhat_bag<-predict(bag,newdata=x_test)
error<-misc(yhat_bag,y_test) #class
error<-mean((yhat_bag-y_test)^2) #regr

### Random Forests ####
library(randomForest)
rf<-randomForest(y~.,data=x_train,importance=TRUE)
importance(rf)
varImpPlot(rf)
yhat_rf<-predict(rf ,newdata=x_test)
rf_error<-misc(yhat_rf,y_test)

### Boosting ####
library(gbm)
boost.out<-gbm(y~.,data=x_train,distribution = "bernoulli",n.tree=100,interaction.depth = 4,
               bag.fraction = 1)
p.hat<-predict(boost.out, x_test, n.trees=100, type= "response")
yhat_boost<-ifelse(p.hat>0.5,1,0)
error<-misc(yhat_boost, y_test)

set.seed()
folds<-sample(1:K, length(train), replace = T) #train=sample(1:n, ceiling..)
B<-c(25,50,100,150)
errcv_b<-matrix(NA,K, length(B))
for (k in 1:k) {
  x.test<-x_train[folds==k,]
  x.train<-x_train[folds!=k,]
  for (i in 1:length(B)){
    boost.out<-gbm(y~.,data=x.train,distribution = "bernoulli",n.tree=B[i],interaction.depth = 4,
                   bag.fraction = 1)
    phat<-predict(boost.out, newdata=x.test, n.tree=B[i], type='response')
    yhat_boost<-ifelse(phat>0.5,1,0)
    errcv_b[k,j]<-misc(yhat, x.test$y)
  }
}
b_best<-B[which.min(colMeans(errcv_b))]
boost.out<-gbm(y~.,data=x_train,distribution = "bernoulli",n.tree=b_best,interaction.depth = 4,
               bag.fraction = 1)
## Support Vector Machines ####
library(e1071)
data<-data.frame(y=as.factor(y),x) #first y<-data[,y]
svmfit<-svm(y~.,data=data,subset=train,kernel ="linear",cost=10) # linear kernel
svmfit<-svm(y~.,data=data,subset=train,kernel ="radial", gamma=1,cost=1) # radial kernel
svmfit<-svm(y~.,data=data, subset=train,kernel ="polynomial", d=3,cost=1) # polynomial kernel
summary(svmfit)

tune_out<-tune(svm,y~.,data=data[train,],ranges=list(cost=c(0.1,1,10,100,1000),kernel="linear"/"polynomial"/"radial")) #train contains y
#kernel="polynomial",d=c(1,2,3,4,5), "radial",gamma=c(1,2,3,4,5)
summary(tune_out)
best_mod<-tune_out$best_model
ypred<-predict(best_mod,x_test) #x_test<-data[.-train,]
error<-misc(ypred, y_test)

###
sort_imp<-imp.var[order(imp.var[,4],decreasing = TRUE),]
which.rf<-rownames(sort_imp)[1:23] #23 coef retained by lasso
which.lasso<-which(lasso.coef!=0)[-1]-1 #to shift for the intercept
which.lasso2<-paste("X",which.lasso,sep="")
sum(which.lasso2 %in%which.rf)
