# prob 1
# tenim en compte que es distribueix de forma normal

mu<- 95.3
sigma<- 5.7

#N -> N(mu,sigma^2)

curve (dnorm(x,mean=mu,sd=sigma),xlim=c(80,120), col='red')     # x valors de barrra   media (mean)    sd(standard deviation)

set.seed()                     # si volguessim els mateixos valors
rnorm(4,mean=mu,sd=sigma)      # simulació. trenquem 4 barres amb aquesta distribució

Y<- function (i) {sum(rnorm(4,mean=mu,sd=sigma))}      # defineixo variable aleatoria 
Y(i)

Y1000<- sapply(1:100000,Y)
mean(Y1000)                       #estimem el valor esperat de trencar 4 barrer inf. vegades

4*mu                            #valor esperat 

hist(Y1000,freq=FALSE)
curve(dnorm(x,4*mu,2*sigma),col='red',add = TRUE )

var100<-100*sigma^2    # tenim una Y amb 100, mu100 = 100mu1   sigma^2 100 = 100*sigma1^2
var100

## apartatc

curve (dnorm(x,mean=mu,sd=sigma),xlim=c(80,120), col='red')             # curva poblacional
1-pnorm(103, mu, sigma)                        #pnorm, area sota la curva

## d

Mm<- function (i) {mean(rnorm(4,mean=mu,sd=sigma))}      # defineixo media muestral 
hist(mm,freq=FALSE)


## e

Ssq<- function (i) {var(rnorm(100,mean=mu,sd=sigma))}      # defineixo variança muestral 
Ssq1000<- sapply(1:100000,Ssq)
hist(Ssq1000,freq=FALSE)

mean(Ssq1000)               


hist(Ssq1000*(100-1)/sigma^2,freq=FALSE)
curve(dchisq(x,100-1),add=TRUE,col='red', freq=FALSE)


