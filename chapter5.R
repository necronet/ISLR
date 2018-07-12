'''
  This script is based con chapter 5 Resampling of the book
  Introduction to Statistical Learning with R. 

  There are a slight modification by removing NA values 
  but the overall technique is the same.

'''

library(ISLR)
library(boot)

set.seed(32)
train = sample(392,196)

lm.fit = lm(mpg~horsepower, data=Auto, subset=train)

AutoDS = na.omit(Auto)

attach(AutoDS)
mean( (mpg-predict(lm.fit,AutoDS))[-train]^2)

lm.fit2 = lm(mpg~poly(horsepower,2), data=AutoDS, subset=train)
mean( (mpg-predict(lm.fit2,AutoDS))[-train]^2)

lm.fit3 = lm(mpg~poly(horsepower,3), data=AutoDS, subset=train)
mean( (mpg-predict(lm.fit3,AutoDS))[-train]^2)

attach(AutoDS)
glm.fit = glm(mpg~horsepower, data=AutoDS)
cv.err=cv.glm(AutoDS,glm.fit)
cv.err$delta

cv.error = rep(0,5)

for(i in 1:5) {
  glm.fit = glm(mpg~poly(horsepower,i),data=AutoDS)
  cv.error[i]=cv.glm(AutoDS,glm.fit)$delta[1]
}

plot(cv.error, col='red', type='o')
lines(cv.error)


# K-fold Cross validation

cv.error.10 = rep(0,10)
for(i in 1:10) {
    glm.fit = glm(mpg~poly(horsepower,i),data=AutoDS)
    cv.error.10[i]=cv.glm(AutoDS,glm.fit,K=10)$delta[1]
}
plot(cv.error.10, col='red')
lines(cv.error.10)

#Bootstraping 

alpha.fn = function(data, index) {
  X = data$X[index]
  Y = data$Y[index]
  return ( (var(Y)-cov(X,Y)) / ( var(X) + var(Y) - 2*cov(X,Y) ) )
}

alpha.fn(Portfolio,1:100)
alpha.fn(Portfolio,sample(100,100,replace=T))

boot(Portfolio, alpha.fn, R=1000)











