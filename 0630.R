g1=function(n,a,b,g){
  x=rnorm(n)
  sum((b-a)*g(a+(b-a)*x)/length(x))
}
n=10000
a=1
b=Inf
g=function(x) {((x^2)*exp(x*x/2))/sqrt(2*pi)}
g1(n,a,b,g)


#（4）
n <- 20
alpha <- .05
mu0 <-500
sigma <- 100
m <- 10000
p <- numeric(m)
for(j in 1:m){
  x <- rnorm(n,mu0,sigma)
  ttest <- t.test(x,alternative = "greater",mu=mu0)
  p[j] <- ttest$p.value
}

p.hat <- mean(p<alpha)
se.hat <- sqrt(p.hat*(1-p.hat)/m)
print(c(p.hat,se.hat))

#（4）
n <- 10
alpha <- .025
m <- 10000
p <- numeric(m)
for(j in 1:m){
  x <- rexp(n,)
  ttest <- t.test(x,alternative = "greater")
  p[j] <- ttest$p.value
}

p.hat <- mean(p<alpha)
se.hat <- sqrt(p.hat*(1-p.hat)/m)
print(c(p.hat,se.hat))


