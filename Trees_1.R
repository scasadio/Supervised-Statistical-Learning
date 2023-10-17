###1.1 regression trees
str(prostate)
x<-prostate[,-ncol(prostate)]
p<-ncol(x)-1
n<-nrow(x)

set.seed(1234)
train<-sample(1:n,ceiling(n/2))
x_test<-x[-train,]

install.packages("tree")
library(tree)
tree_prostate<-tree(lpsa~.,x,subset=train)
summary(tree_prostate)
#49-7 degrees of freedom

tree_prostate
plot(tree_prostate)
text(tree_prostate, digits=3)
tree_pred<-predict(tree_prostate, x_test)
mean((tree_pred-x_test$lpsa)^2) #mse for the first tree we fitted
#the tree overfit the data, no terminal nodes with more than 9 units
#perform cross validation with cv function

?cv.tree
set.seed(1234)
cvtree_prostate<-cv.tree(tree_prostate, K=5, FUN=prune.tree)
cvtree_prostate

best_terminal<-cvtree_prostate$size[which.min(cvtree_prostate$dev)]
best_terminal

prune_prostate<-prune.tree(tree_prostate, best=best_terminal)
plot(prune_prostate)
text(prune_prostate)
cvtree_pred<-predict(prune_prostate,x_test)
mean((cvtree_pred-x_test$lpsa)^2)

###1.2 classification trees
str(SAheart)
n<-nrow(SAheart)
y<-SAheart$chd
x<-SAheart[,-10]
str(x)

heart<-data.frame(chd=as.factor(y),x)#we need to transorfm chd in a factor for a good interpretation
str(heart)

set.seed(1234)
train<-sample(1:n,ceiling(n/2))
heart_test<-heart[-train,]

tree_heart<-tree(chd~.,heart,subset = train)
summary(tree_heart)
#misclassification error rate=misclassficated units/total units

plot(tree_heart)
text(tree_heart, pretty=0)#with pretty =0, lables are unchanged, famhist:absent

?predict.tree
tree_pred<-predict(tree_heart,heart_test,type="class")

misc<-function(y_hat,y){
  1-sum(diag(table(y_hat,y)))/length(y)
}
misc(tree_pred,heart_test$chd)

set.seed(1234)
cv_heart<-cv.tree(tree_heart, FUN=prune.misclass)
cv_heart

prune_heart<-prune.misclass(tree_heart,best = 5)
plot(prune_heart)
text(prune_heart,pretty = 0)

tree_pred<-predict(prune_heart, heart_test,type = "class")
table(tree_pred, heart_test$chd)

1-misc(tree_pred,heart_test$chd)

prune_heart2<-prune.misclass(tree_heart, best=15)
plot(prune_heart2)
text(prune_heart2,heart_test$chd)

tree_pred2<-predict(prune_heart2,heart_test,type="class")
table(tree_pred2,heart_test$chd)

1-misc(tree_pred2,heart_test$chd)

###2 bagging and random forests
installed.packages("randomForest")
library(randomForest)

set.seed(1234)

bag.heart<-randomForest(chd~.,heart,subset=train,mtry_ncol(heart)-1,importance=TRUE)
#34,63% train error rate (?)

importance(bag.heart)

varImpPlot(bag.heart)
yhat.bag<-predict(bag.heart,newdata=heart[-train,])
table(yhat.bag,heart_test$chd)
misc(yhat.bag,heart_test$chd)

###
set.seed(1234)
rf.heart<-randomForest(chd~.,heart,subset=train, importance=TRUE)

importance(rf.heart)

varImpPlot(rf.heart)
yhat.rf<-predict(rf.heart,newdata=heart[-train,])
table(yhat.rf,heart_test$chd)
misc(yhat.rf,heart_test$chd) #32.6% of units are misclassified, so better than bagging
#consider a smaller number of units improve the accuracy with respesct to the random forest