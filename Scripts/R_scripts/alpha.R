f <- function(x,alpha){
  x = x^alpha
  
}
y <- mat.or.vec(1,100)
x<- seq(0.01,1,0.01)
length(x)
plot(-100,-100,xlim=c(0,1),ylim=c(0,1))
valores=c(0.0001,0.001,0.01,0.1,1)
for(j in 1:length(valores)){
  alpha=valores[j]
  for(i in 1:100){
   y[i]=f(x[i],alpha)
  }
  lines(x,y,col=j)
}
    