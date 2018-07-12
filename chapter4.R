library(ISLR)
names(Smarket)

dim(Smarket)
summary(Smarket)
pairs(Smarket)

cor(Smarket[,-9])

attach(Smarket)
plot(Volume)

glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fit)

coef(glm.fit)

glm.probs = predict(glm.fit, type="response")
glm.probs[1:10]

contrasts(Direction)


glm.pred = rep("Down",1250)
glm.pred[glm.probs>0.5] = "Up"

table(glm.pred, Direction)

mean(glm.pred == Direction)


train = (Year<2005)
Smarket.2005 = Smarket[!train,]


### LDA example
library(MASS)
Direction.2005 = Direction[!train]
lda.fit = lda(Direction~Lag1+Lag2, data=Smarket,subset=train)
lda.pred = predict(lda.fit, Smarket.2005)

lda.class = lda.pred$class

mean(lda.class == Direction.2005)
sum(lda.pred$posterior[,1]>=0.5)
sum(lda.pred$posterior[,1] < 0.5)

lda.pred$posterior[1:20,1]
lda.pred$class[1:20]

# QDA 
qda.fit = qda(Direction~Lag1+Lag2, data=Smarket,subset=train)
qda.class = predict(qda.fit, Smarket.2005)$class

table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)


#KNN

library(class)

train.X = cbind(Lag1,Lag2)[train,]
test.X = cbind(Lag1,Lag2)[!train,]
train.Direction = Direction[train]

set.seed(35751)
knn.pred = knn(train.X,test.X,train.Direction,k=1)
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005)

knn.pred = knn(train.X,test.X,train.Direction,k=3)
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005)


## Caravan example
attach(Caravan)
summary(Purchase)

standarized.X = scale(Caravan[,-86])

test=1:1000
train.X = standarized.X[-test,]
test.X = standarized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]

knn.pred = knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)

mean(test.Y!="No")
table(knn.pred, test.Y)

knn.pred = knn(train.X,test.X,train.Y,k=2)
table(knn.pred, test.Y)






