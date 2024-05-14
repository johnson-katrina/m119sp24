p <- function(x,lambda){lambda^x/factorial(x)*exp(-lambda)}

p(1)
p(1,1)
p(0,1)
1-(p(0,1)+p(1,1))


p <- function(x,lambda=2){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}
lambda <- seq(1e-10,20,0.1)
out.p <- p(7,lambda)
plot(lambda,out.p,type='l')
abline(v=7,col=2)
