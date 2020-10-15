tam=1000
tiempo=100
prob=mat.or.vec(3,1)
prob=c(1/3,2/3,3/3)

prob=c(.05,.065,1)
prob=c(0,1,0)
v = vector(mode="integer", tam)

i=0
for(i in 1:tam){
    v[i]=20
    i=i+1
}
v
plot(density(v),xlim=c(0,50))
i=0
for(i in 1:tiempo){
  for(j in 1:tam){
    x=v[j]
    prob=c(x/2000,x/1000,1)
    p = runif(1,0,1)
    if(p<prob[1]){
    q=rgeom(1,1/x)+1
    if(q>=x){q=x-1}
    #q=1
    x=x+q
  }else{
    if(p<prob[2]){
      q=rgeom(1,1/x)+1
      if(q>=x){q=x-1}
      #q=1
      x=x-q
      }
  }
  #if(x<4){x=4}
  #if(x > 20){x=20}
    v[j]=x
  j=j+1
  }
  if(i %% 5 == 0){lines(density(v),col=(i/5))
    abline(v=mean(v),col=(i/5))
    #promedios(i/10,)
    }
    
  i=i+1
}

