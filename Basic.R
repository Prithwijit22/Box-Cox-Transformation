library(ggplot2)
##Graph
f = function(t,l){
  if(l==0)
    z = exp(t)
  else
    z = (l*t+1)^(1/l)
  return(z)
}

x = seq(0,2,by = 0.001)
l0 = f(t = x,l = 0)
l1 = f(t = x,l = 1)
l2 = f(t = x,l = 2)
l_1 = f(t = x,l = -1)
l_2 = f(t = x,l = -2)
df = data.frame(x,l0,l1,l2,l_1,l_2)

p = ggplot()
p+geom_line(data = df,mapping = aes(x,l0),color = "red")+geom_line(data = df,mapping = aes(x,l1),color = "blue")+
  geom_line(data = df,mapping = aes(x,l_1),color = "grey")+geom_line(data = df,mapping = aes(x,l2),color = "pink")+
  geom_line(data = df,mapping = aes(x,l_2),color = "green")+ylim(-500,500)+
  
