#EXAM#

#title: 'exercise 7'
#author:'Sara Casadio'
#output: html_document

load("C:/Users/Utente/Downloads/MockExam (1).RData")

#Load the workspace MockExam.RData into your R environment. 
#Using command ls() you will see an object named genes. Such matrix  includes 40 tissue samples 
#with gene expression measurements on 1,000 genes. The first 20 samples are from healthy patients (class=1), 
#while the second 20 are from a diseased group (class=2) [labels are included in object genes_labs].

##1###
#Apply the Lasso on the subset of units identified by train to perform variable selection of the model 
#that has genes_labs as response. How many predictors are retained? Compute the test error estimate.
ls()
library(glmnet)

y<-as.factor(genes_labs[train])
x<-as.matrix(genes[train,])
test.x<-as.matrix(genes[-train,])

out.mod<-glmnet(x,y,alpha=1,family="binomial")

set.seed(1234)
cv.lasso<-cv.glmnet(x,y,alpha=1,family="binomial")
best.lambda<-cv.lasso$lambda.min

lasso.coef<-predict(out.mod,s=best.lambda,type="coefficients")
sum(lasso.coef!=0)
## [1] 24  #lasso regularization retains 23 predictor, one is for the intercept

lasso.pred<-predict(out.mod,s=best.lambda,type="class", newx=test.x)
#check the misclassification error
mean(lasso.pred!=genes_labs[-train]) #0 error, perfect fit


##2###
#Run a random forest on the training set. Evaluate the importance of the genes in terms of average decrease of Gini index. 
#Why is that measure connected with the variable importance? 
install.packages('randomForest')
library(randomForest)
df<-data.frame(y=as.factor(genes_labs),genes)

set.seed(1234)
out.rf<-randomForest(y~.,df,subset=train, importance=TRUE) 

imp.var<-out.rf$importance
#to have the most important on the top
sorted.var<-rownames(imp.var)[order(imp.var[,4],decreasing = T),]


##3###
#Retain a number of best predictors equal to that identified by the lasso. How many genes do the two methods have in common?
b.lasso<-which(lasso.coeff!=0)[-1] #intercept


##4###
#Compute the test error estimate of the random forest classifier.

yhat.rf<-predict(out.rf,newdata=df[-train,])
mean(yhat.rf!=df$y[-train])

##5###
#In this case, which classification method would you prefer and why?
#they return the same accuracy but i prefere lasso or logistic regression because at least 
#i can tell if a variable has a significant effect on the respose variable or not
