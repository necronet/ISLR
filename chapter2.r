set.seed(28953723)

x = rnorm(100, mean=0)
y = rnorm(100, mean=0)

plot(x,y,xlab="X norm",ylab="Y norm",main="Plot X vs Y", col="red")


#countour
x <- seq(-pi,pi,length=150)
y <- seq(-pi,pi,length=150)
f <- outer(x, y, function(x,y) cos(x)/(1+y^2))
contour(x,y,f)
contour(x,y,f, nlevels=45, add=T)
fa = (f-t(f))/2
contour(x,y,fa,nlevels=15)

image(x,y,fa)
image(x,y,f)
persp(x,y,f)
persp(x,y,fa,theta=30,phi=20)


A = matrix(1:16, 4,4)

A[c(1,3),c(2,4)]

Auto = read.table('Auto.data', header=T, na.strings="?")
# dim(Auto)
plot(Auto$cylinders, Auto$mpg)


plot(Auto$cylinders, Auto$mpg, col="red", varwidth=T, horizontal=T)

hist(Auto$mpg, col=2, breaks=15)





