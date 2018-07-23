'''
  ISLR example from chapter 6 with some minor tweaks here and there
'''
library(ISLR)
library(leaps)
library(glmnet)

names(Hitters)
dim(Hitters)

sum(is.na(Hitters$Salary))

Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

regfit.full = regsubsets(Salary~.,Hitters)
summary(regfit.full)

regfit.full = regsubsets(Salary~., data=Hitters,nvmax=19)
summary(regfit.full)

reg.summary = summary(regfit.full)

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of variables", ylab="RSS", type="l")
minpoint = which.min(reg.summary$rss)
points(minpoint,reg.summary$rss[19], col="red", cex=2, pch=20)

maxpoint = which.max(reg.summary$adjr2)
plot(reg.summary$adjr2, xlab="Number of variables", ylab="Adj. RSS", type="l")
points(maxpoint,reg.summary$adjr2[maxpoint], col="red", cex=2, pch=20)


# Forward and Backward selection
regfit.fwd = regsubsets(Salary ~., data=Hitters,nvmax=19, method="forward")
summary(regfit.fwd)

regfit.bwd = regsubsets(Salary ~., data=Hitters,nvmax=19, method="backward")
summary(regfit.bwd)

# Cross validation for model selection

set.seed(1)
train = sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)
test = !train
regfit.best = regsubsets(Salary ~., data=Hitters[train,],nvmax=19)

test.mat = model.matrix(Salary~., data=Hitters[test,])

val.errors =rep(NA ,19)

for(i in 1:19){ 
  coefi = coef(regfit.best,id=i) 
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2 )
  
}

coef(regfit.best,which.min(val.errors))

predict.regsubsets = function(object,newdata,id,...){
 form=as.formula(object$call[[2]])
 mat=model.matrix(form,newdata)
 coefi=coef(object,id=id)
 xvars=names(coefi)
 mat[,xvars]%*%coefi
}

regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)

# Cross validation

k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters),replace=TRUE)
cv.errors = matrix(NA,k,19,dimnames = list(NULL,paste(1:19)))


for(j in 1:k) { 
  best.fit = regsubsets(Salary~., data=Hitters[folds!=j,], nvmax=19)  
  for( i in 1:19) {
    pred = predict(best.fit,Hitters[folds==j,], id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}

mean.cv.errors = apply(cv.errors,2,mean)
minIndex = which.min(mean.cv.errors)
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')

reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,minIndex)


#Ridge regression and Lasso

x = model.matrix(Salary~.,Hitters)[,-1]
y = Hitters$Salary
grid = 10^seq(10,-2,length=100)

ridge.mod = glmnet(x,y,alpha=0,lambda=grid)
coef(ridge.mod)
predict(ridge.mod, s=50,type="coefficients")[1:20,]

set.seed(1)
train = sample(1:nrow(x),nrow(x)/2)
test = -train
y.test = y[test]

ridge.mod = glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred = predict(ridge.mod,s=4,newx=x[test,])
mean( (ridge.pred-y.test)^2 )

ridge.pred=predict(ridge.mod ,s=1e10 ,newx=x[test ,])
mean((ridge.pred -y.test)^2)

cv.out=cv.glmnet(x[train ,],y[ train],alpha=0)
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam

ridge.pred=predict (ridge.mod ,s=bestlam ,newx=x[test ,])
mean((ridge.pred -y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s= bestlam) [1:20,]


## The Lasso

lasso.mod=glmnet(x[train ,],y[ train],alpha=1, lambda =grid)
plot(lasso.mod)
cv.out=cv.glmnet(x[train ,],y[ train],alpha=1)
plot(cv.out)
bestlam =cv.out$lambda.min
lasso.pred=predict (lasso.mod ,s=bestlam ,newx=x[test ,])
mean((lasso.pred -y.test)^2)

out=glmnet (x,y,alpha=1, lambda=grid)
lasso.coef=predict (out ,type="coefficients",s= bestlam) [1:20,]


# Principal Component Regression

library(pls)
pcr.fit = pcr(Salary~., data=Hitters, scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")

pcr.fit = pcr(Salary~., data=Hitters, scale=TRUE, validation="CV", subset=train)
validationplot(pcr.fit, val.type="MSEP")
pcr.pred=predict(pcr.fit, x[test,] ,ncomp=7)
mean((pcr.pred-y.test)^2)

# Partial Least Square

set.seed(1)
pls.fit = plsr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")
pls.pred = predict(pls.fit, x[test,], ncomp=2)
mean((pls.pred-y.test)^2)

pls.fit = plsr(Salary~., data=Hitters, scale=TRUE, validation="CV")
summary(pls.fit)
