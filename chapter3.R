library(MASS)
library(ISLR)
library(car)
library(corrplot)

#fix(Boston)
names(Boston)

attach(Boston)
lm.fit = lm(medv~lstat)
summary(lm.fit)


par(mfrow=c(2,2))
plot(lstat,medv,pch="+",col="red")
abline(lm.fit)

plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))


# Multiple linear regression

lm.fit = lm(medv~lstat+age,data=Boston)
summary(lm.fit)

lm.fit = lm(medv~.,data=Boston)
summary(lm.fit)
vif(lm.fit)
corrplot(cor(Boston))
pairs(~medv+crim+zn+nox+chas+age, head(Boston,100))

# summary(lm.fit)$sigma
# summary(lm.fit)$r.sq

lm.fit1 = lm(medv~.-age,data=Boston)
summary(lm.fit1)

summary(lm(medv~lstat*age,data=Boston))

lm.fit2 = lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

lm.fit = lm(medv~lstat)
anova(lm.fit,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5 = lm(medv~poly(lstat,5))
summary(lm.fit5)

summary(lm(medv~log(rm)))


lm.fit = lm










