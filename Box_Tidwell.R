library(mvtnorm)
library(ggplot2)
library(car)
library(carData)

##1.Example
x = rnorm(1000,10,1)
e = rnorm(1000,0,5)
y = 5+2*x^2+e
D = data.frame(x,y)

hist(y)

##SLR
Model = lm(y~x)
summary(Model)
y_lm = Model$fitted.values
boxTidwell(y~x,data = D)
e_lm = y-y_lm
d1 = data.frame(y_lm,e_lm)
ggplot(d1, aes(y_lm,e_lm)) + geom_point()





m = lm(y~I(x^2))
a = m$coefficients[1]
b = m$coefficients[2]
Y_new = m$fitted.values
summary(m)
e_new = y - Y_new
d2 = data.frame(Y_new,e_new)
ggplot(d2, aes(Y_new,e_new)) + geom_point()



hist(Y_new)








###MLR
e = rnorm(1000,0,5)
X = abs(rmvnorm(1000,numeric(4)+20,diag(4)))
X[,2] = X[,2]^2
beta = runif(4,0,2)
beta[2] = 2
Y = X%*%beta + e
DF = data.frame(X,Y)

hist(Y)
MModel = lm(Y~.,data = DF)
summary(MModel)

y_lm = MModel$fitted.values
e_lm = y-y_lm
d1 = data.frame(y_lm,e_lm)
ggplot(d1, aes(y_lm,e_lm)) + geom_point()




G1 = boxTidwell(Y~X1,data = DF)
wt1 = G1$result[,1]
g1 = lm(Y~I(X1^wt1[1]) + X2 + X3 + X4,data = DF)
summary(g1)

G2 = boxTidwell(Y~X2,data = DF)
wt2 = G2$result[,1]
g2 = lm(Y~I(X2^wt2[1]) + X1 + X3 + X4,data = DF)
summary(g2)

G3 = boxTidwell(Y~X3,data = DF)
wt3 = G3$result[,1]
g3 = lm(Y~I(X3^wt3[1]) + X2 + X1 + X4,data = DF)
summary(g3)

G4 = boxTidwell(Y~X4,data = DF)
wt4 = G4$result[,1]
g4 = lm(Y~I(X4^wt4[1]) + X2 + X3 + X1,data = DF)
summary(g4)


G11 = boxTidwell(Y~X1+X2,data = DF)
wt11 = G11$result[,1]
g11 = lm(Y~I(X1^wt1[1]) + I(X2^wt11[2]) + X3 + X4,data = DF)
summary(g11)

G12 = boxTidwell(Y~X1+X3,data = DF)
wt12 = G12$result[,1]
g12 = lm(Y~I(X1^wt12[1]) + X2 + I(X3^wt12[2]) + X4,data = DF)
summary(g12)

G13 = boxTidwell(Y~X1+X4,data = DF)
wt13 = G13$result[,1]
g13 = lm(Y~I(X1^wt13[1]) + X2 + I(X4^wt13[2]) + X3,data = DF)
summary(g13)

G14 = boxTidwell(Y~X3+X4,data = DF)
wt14 = G14$result[,1]
g14 = lm(Y~I(X3^wt14[1]) + X1 + I(X4^wt14[2]) + X2,data = DF)
summary(g14)

G15 = boxTidwell(Y~X2+X3,data = DF)
wt15 = G15$result[,1]
g15 = lm(Y~I(X2^wt14[1]) + X1 + I(X3^wt14[2]) + X4,data = DF)
summary(g15)

G16 = boxTidwell(Y~X2+X4,data = DF)
wt16 = G16$result[,1]
g16 = lm(Y~I(X2^wt16[1]) + X1 + I(X4^wt16[2]) + X3,data = DF)
summary(g16)


G111 = boxTidwell(Y~X1+X2+X3,data = DF)
wt111 = G111$result[,1]
g111 = lm(Y~I(X1^wt111[1]) + I(X2^wt111[2]) + I(X3^wt111[3]) + X4,data = DF)
summary(g111)


G112 = boxTidwell(Y~X1+X2+X4,data = DF)
wt112 = G112$result[,1]
g112 = lm(Y~I(X1^wt112[1]) + I(X2^wt112[2]) + I(X4^wt112[3]) + X3,data = DF)
summary(g112)

G113 = boxTidwell(Y~X1+X3+X4,data = DF)
wt113 = G113$result[,1]
g113 = lm(Y~I(X1^wt113[1]) + I(X3^wt113[2]) + I(X4^wt113[3]) + X2,data = DF)
summary(g113)

G114 = boxTidwell(Y~X2+X3+X4,data = DF)
wt114 = G114$result[,1]
g114 = lm(Y~I(X2^wt114[1]) + I(X3^wt114[2]) + I(X4^wt114[3]) + X1,data = DF)
summary(g114)

G = boxTidwell(Y~X1+X2+X3+X4,data = DF)
wt = G$result[,1]
g = lm(Y~I(X1^wt[1]) + I(X2^wt[2]) + I(X3^wt[3]) + I(X4^wt[4]),data = DF)
summary(g)
Y_new = g$fitted.values
e_new = Y - Y_new
d2 = data.frame(Y_new,e_new)
ggplot(d2, aes(Y_new,e_new)) + geom_point()


hist(Y_new)










##Real Data
D = data("Prestige")
D = data.frame(Prestige)
D = D[,-ncol(D)]
prestige = D$prestige

Model = lm(prestige~.,data = D)
summary(Model)
y_lm = Model$fitted.values
e_lm = prestige - y_lm
d1 = data.frame(y_lm,e_lm)
ggplot(d1, aes(y_lm,e_lm)) + geom_point()


c = median(D[,3])
D[c(58,69,87,97,100),3] = c

P1 = boxTidwell(prestige~income,~women+education,data = D)
wt1 = P1$result[,1]
m1 = lm(prestige~I(income^wt1[1])+I(education)+I(women),data = D)
summary(m1)



P2 = boxTidwell(prestige~women,~income+education,data = D)
wt2 = P2$result[,1]
m2 = lm(prestige~I(women^wt2[1])+I(education)+I(income),data = D)
summary(m2)


P3 = boxTidwell(prestige~education,~women,data = D)
wt3 = P3$result[,1]
m3 = lm(prestige~I(women)+I(education^wt3[1])+I(income),data = D)
summary(m3)


P11 = boxTidwell(prestige~income+women,~education,data = D)
wt11 = P11$result[,1]
m11 = lm(prestige~I(income^wt11[1])+I(education)+I(women^wt11[2]),data = D)
summary(m11)


P12 = boxTidwell(prestige~income+education,~women,data = D)
wt12 = P12$result[,1]
m12 = lm(prestige~I(income^wt12[1])+I(education^wt12[2])+I(women),data = D)
summary(m12)


P13 = boxTidwell(prestige~women+education,~income,data = D)
wt13 = P13$result[,1]
m13 = lm(prestige~I(income)+I(education^wt13[2])+I(women^wt13[1]),data = D)
summary(m13)


P111 = boxTidwell(prestige~women+education+income,data = D)
wt111 = P111$result[,1]
m111 = lm(prestige~I(income^wt111[3])+I(education^wt111[2])+I(women^wt111[1]),data = D)
summary(m111)


P11 = boxTidwell(prestige~income+women,~education,data = D)
wt11 = P11$result[,1]
m11 = lm(prestige~I(income^wt11[1])+I(education)+I(women^wt11[2]),data = D)
summary(m11)
Y_new = m11$fitted.values

summary(m11)
hist(Y_new)
e_new = prestige - Y_new
plot(Y_new,e_new)
d2 = data.frame(Y_new,e_new)
ggplot(d2, aes(Y_new,e_new)) + geom_point()
hist(Y_new)

