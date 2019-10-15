del=0.5
n=1000				#values of delta,no. of steps, correlation
givencor=.9

x1 = 1
x2 = 1				#initial values

X1 = c(x1)
X2 = c(x2)				#Array storing x1 & x2


count = 0
accept = 0
reject = 0
while(count<n){

xnew = runif(1, min = x1-del, max = x1+del)
ynew = runif(1, min = x2-del, max = x2+del)

newcor = cor(c(X1,xnew), c(X2,ynew))
if(newcor >= givencor){					#if() then accept those points

X1 = c(X1,xnew)
X2 = c(X2,ynew)						#push into array
count = count + 1
x1 = xnew							#now accepted point is initial
x2 = ynew							#point
accept = accept+1

}
else{								#otherwise reject it
reject = reject+1
}
#count=count+1
}
accept
reject
poa =(accept/(accept+reject))*100			#prob. of acceptance
plot(X1,X2)							#plotting all accepted values
poa