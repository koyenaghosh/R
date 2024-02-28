#Ztest
#calculate cut off
set.seed(10)
mew_0=2
sigma=1
n=20
N=50000
z=array(dim = 1)
for (i in 1:N) {
  x=rnorm(20,mew_0,sigma)
  z[i]=sqrt(n)*(mean(x)-mew_0)/sigma
}
c=quantile(z,0.95)
c
#calculate power
mew_1=array(seq(2,2.9,0.1))
length(mew_1)
for (j in 1:length(mew_1)) 
{
count=0
  for (i in 1:N) 
  {
    x=rnorm(20,mew_1[j],sigma)
    z[i]=sqrt(n)*(mean(x)-mew_0)/sigma
    if (z[i]>c) 
      {
      count=count+1
      }
  }
  
power[j]=count/N
}
power
plot(mew_1,power,type = "l")
