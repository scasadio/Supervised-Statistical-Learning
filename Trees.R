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
tree_prostate

plot(tree_prostate)
text(tree_prostate,digits=3,pretty = 0)
tree_pred<-predict(tree_prostate, x_test)
mean((tree_pred  - x_test$lpsa)^2)

?cv.tree
set.seed(1234)
cvtree_prostate<-cv.tree(tree_prostate, K=5, FUN=prune.tree)
cvtree_prostate

best_terminal<-cvtree_prostate$size[which.min(cvtree_prostate$dev)]
best_terminal

?prune.tree
prune_prostate<-prune.tree(tree_prostate,best=best_terminal)
plot(prune_prostate)
text(prune_prostate)

cvtree_pred<-predict(prune_prostate,x_test)
mean((cvtree_pred  - x_test$lpsa)^2)

#########

str(SAheart)
n<-nrow(SAheart)
y<-SAheart$chd
x<-SAheart[,-10]
str(x)

heart<-data.frame(chd=as.factor(y),x)
str(heart)

set.seed(1234)
train<-sample(1:n,ceiling(n/2))
heart_test<-heart[-train,]

tree_heart<-tree(chd~.,heart,subset=train)
summary(tree_heart)

plot(tree_heart)
text(tree_heart,pretty=0)
?text.tree

?predict.tree
tree_pred<-predict(tree_heart,heart_test,type="class")

table(tree_pred,heart_test$chd)

misc<-function(y_hat,y){
 1-sum(diag(table(y_hat,y)))/length(y) 
}

misc(tree_pred,heart_test$chd)
1-misc(tree_pred,heart_test$chd) #correct prediction is around 60%

set.seed(1234)
cv_heart<-cv.tree(tree_heart,FUN=prune.misclass)
cv_heart

cv_heart$size[which.min(cv_heart$dev)]

prune_heart<-prune.misclass(tree_heart,best=5)
plot(prune_heart)
text(prune_heart,pretty=0)

tree_pred<-predict(prune_heart,heart_test,type="class")
table(tree_pred,heart_test$chd)

1-misc(tree_pred,heart_test$chd)

prune_heart2<-prune.misclass(tree_heart,best=15)
plot(prune_heart2)
text(prune_heart2,pretty=0)

tree_pred2<-predict(prune_heart2,heart_test,type="class")
table(tree_pred2,heart_test$chd)

1-misc(tree_pred2,heart_test$chd)

###################

install.packages("randomForest")
library(randomForest)

set.seed(1234)
?randomForest

bag.heart<-randomForest(chd~.,heart,subset=train,mtry=ncol(heart)-1,importance=TRUE)
bag.heart

importance(bag.heart)
?importance

varImpPlot(bag.heart)
yhat.bag<-predict(bag.heart,newdata=heart[-train,])
table(yhat.bag,heart_test$chd)
misc(yhat.bag,heart_test$chd)

#################

set.seed(1234)
rf.heart<-randomForest(chd~.,heart,subset=train,importance=TRUE)
rf.heart

importance(rf.heart)
?importance

varImpPlot(rf.heart)
yhat.rf<-predict(rf.heart,newdata=heart[-train,])
table(yhat.rf,heart_test$chd)
misc(yhat.rf,heart_test$chd)

