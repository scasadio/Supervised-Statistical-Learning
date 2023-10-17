# Regularization and Dimension Reduction - May 13th 2022 #####

## Ridge regression ####e
#install.packages("ElemStatLearn")
#library(ElemStatLearn)

load("C:/Users/Utente/OneDrive/Desktop/LM/SUPERVISED/prostate.RData")
data(prostate)
summary(prostate)

x<-prostate[,-ncol(prostate)]
y<-prostate$lpsa
p<-ncol(x)-1

install.packages('glmnet')
library(glmnet)

x<-model.matrix(lpsa~.,x)[,-1]
head(x)
grid<-10^seq(10,-2,length=100)
ridge.mod<-glmnet(x,y,alpha=0,lambda = grid)

dim(coef(ridge.mod))
coef(ridge.mod)[,50]
ridge.mod$lambda[50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

coef(ridge.mod)[,60]
ridge.mod$lambda[60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod,s=50, type ="coefficients")[1:9,]

## Training + Validation set
set.seed(1234)
train<-sample(1:nrow(x),ceiling(nrow(x)/2))
y.test<-y[-train]

#test<-(-train )
ridge.mod<-glmnet(x[train,],y[train],alpha=0,
                  lambda=grid)
ridge.pred<-predict(ridge.mod,s=4,newx=x[-train,]) #newx=x[test,]
mean((y.test-ridge.pred)^2)

mean((mean(y[train])-y.test)^2)

# Compare vector of regularized regression coefficients with
# that of OLS
ridge.pred<-predict(ridge.mod,s=0,exact=T,
                    x=x[train,],y=y[train],newx=x[-train,])
mean((ridge.pred-y.test)^2)
lm(y~x,subset = train)

predict(ridge.mod, s=0, exact =T,x=x[train,],y=y[train],type="coefficients")[1:9,]

## Choice of lambda via 10-fold CV
set.seed(1234)
cv.out<-cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
best.lambda<-cv.out$lambda.min
best.lambda

ridge.pred<-predict(ridge.mod,s=best.lambda,newx = x[-train,],
                    exact=T, x=x[train,],y=y[train])
mean((y.test-ridge.pred)^2)

predict(ridge.mod,s=best.lambda,newx = x[-train,],
        exact=T, x=x[train,],y=y[train],type="coefficients")[1:9,]

## Lasso Regression ####
lasso.mod<-glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

set.seed(1234)
cv.out<-cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)

best.lambda<-cv.out$lambda.min
lasso.pred<-predict(lasso.mod,s=best.lambda,newx=x[-train,],
                    exact=T,x=x[train,],y=y[train])
mean((y.test-lasso.pred)^2)

out<-glmnet(x, y, alpha=1, lambda=grid)
lasso.coef=predict(out, type ="coefficients",s=best.lambda)[1:9,]
lasso.coef

# Dimension reduction ####

## PCR ####
install.packages('pls')
library(pls)

set.seed(1234)
pcr.fit<-pcr(lpsa~.,data=prostate[,-ncol(prostate)],scale=T,validation="CV")
summary(pcr.fit)

validationplot(pcr.fit)

ncomp.1sigma<-selectNcomp(pcr.fit,method="onesigma",plot=T)
ncomp.random<-selectNcomp(pcr.fit,method="randomization",plot=T)


# Test error estimate ####
set.seed(1234)
train<-sample(1:nrow(x),nrow(x)/2)
y.test<-prostate[-train,9]
pcr.fit<-pcr(lpsa~.,data=prostate[,-ncol(prostate)],
             subset=train,scale=T,validation="CV")
validationplot(pcr.fit,val.type="MSEP", legendpos= "top")
summary(pcr.fit)
pcr.pred<-predict(pcr.fit,prostate[-train,1:8],ncomp=5)
mean((pcr.pred-y.test)^2)

# PCR via eigen()
x.pcs<-eigen(cor(prostate[train,1:8]))$vectors
test.x<-prostate[-train,1:8]
for(i in 1:8){
test.x[,i]<-(test.x[,i]-mean(prostate[train,i]))/sd(prostate[train,i])
}
x.train<-scale(prostate[train,1:8],T,T)%*%x.pcs[,1:5]
x.test<-as.matrix(test.x)%*%x.pcs[,1:5]
y.train<-prostate[train,]$lpsa
y.test<-prostate[-train,]$lpsa
data.pcs<-data.frame(y=c(y.train,y.test),rbind(x.train,x.test))
out.pcs<-lm(y~.,data.pcs,subset = 1:length(train))
y.hat<-predict(out.pcs,newdata = data.pcs[(length(train)+1):nrow(data.pcs),-1])
mean((y.hat-y.test)^2)

# PCR via svd()
xx<-scale(prostate[train,1:8],T,T)
svd.xx<-svd(xx)
xx.pcs<-svd.xx$v[,1:5]
test.xx<-prostate[-train,1:8]
for(i in 1:8){
test.xx[,i]<-(test.xx[,i]-mean(prostate[train,i]))/sd(prostate[train,i])
}
xx.train<-xx%*%xx.pcs
xx.test<-as.matrix(test.xx)%*%xx.pcs[,1:5]
y.train<-prostate[train,]$lpsa
y.test<-prostate[-train,]$lpsa
data.svd<-data.frame(y=c(y.train,y.test),rbind(xx.train,xx.test))
out.pcs<-lm(y~.,data.svd,subset = 1:length(train))
yy.hat<-predict(out.pcs,newdata = data.svd[(length(train)+1):nrow(data.svd),-1])
mean((yy.hat-y.test)^2)

pcr.fit<-pcr(y~x, scale=TRUE, ncomp=5)
summary(pcr.fit)
