library(EnvStats)
library(ggplot2)
##1.Example
x = rnorm(1000,10)
e = rnorm(1000,0,2)
y = exp(5+2*x)+e
D = data.frame(x,y)
d1 = data.frame(y)
p1 = ggplot(d1, aes(sample = y))
p1 + stat_qq() + stat_qq_line()
shapiro.test(y)
 hist(y)


Model = lm(y~x,data = D)
P = Model$coefficients
summary(Model)

B = boxcox(Model,optimize = TRUE,objective.name = "Shapiro-Wilk")
l = B$lambda
l
Y = boxcoxTransform(y,B$lambda)
m = lm(Y~x)
Y_new = m$fitted.values
summary(m)

d2 = data.frame(Y_new)
p2 = ggplot(d2, aes(sample = Y_new))
p2 + stat_qq() + stat_qq_line()
shapiro.test(Y_new)

hist(Y_new)

f = function(t,l){
  if(l==0)
    z = exp(t)
  else
    z = (l*t+1)^(1/l)
  return(z)
}


x1 = seq(5,15,length.out = 1000)
fit = f(t=x1,l = l)
df = data.frame(x1,fit)

ggplot() + geom_point(data = D,mapping = aes(x,y))


##2.Example
x = rnorm(1000,100,1)
e = rnorm(1000,0,2)
y = (1/8)*(5+2*x)^8+e
D = data.frame(x,y)
d1 = data.frame(y)
p1 = ggplot(d1, aes(sample = y))
p1 + stat_qq() + stat_qq_line()
shapiro.test(y)
hist(y)

Model = lm(y~x,data = D)
P = Model$coefficients
summary(Model)

B = boxcox(Model,optimize = TRUE,objective.name = "Shapiro-Wilk")
B
l = B$lambda
Y = boxcoxTransform(y,B$lambda)
m = lm(Y~x)
Y_new = m$fitted.values
summary(m)
d2 = data.frame(Y_new)
p2 = ggplot(d2, aes(sample = Y_new))
p2 + stat_qq() + stat_qq_line()
shapiro.test(Y_new)
hist(Y_new)
ggplot() + geom_point(data = D,mapping = aes(x,y))






##Real Data
D = data("trees")
D = trees
#View(D)
Volume = D$Volume
d1 = data.frame(Volume)
p1 = ggplot(d1, aes(sample = Volume))
p1 + stat_qq() + stat_qq_line()
shapiro.test(Volume)
hist(Volume)


M  = lm(Volume~.,data = D)
summary(M)

b = boxcox(M,optimize = TRUE,objective.name = "Shapiro-Wilk")
w = b$lambda
w
Volume_p = boxcoxTransform(D$Volume,w)
m = lm(Volume_p~.,data = D)
Y_new = m$fitted.values
d2 = data.frame(Y_new)
p2 = ggplot(d2, aes(sample = Y_new))
p2 + stat_qq() + stat_qq_line()
shapiro.test(Y_new)
summary(m)
hist(Y_new)







