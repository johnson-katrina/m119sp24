y <- function(x){8/x}
f <- function(x){sqrt(x^2 + 64/(x^2))}

x <- seq(-5,5,0.01)

plot(x,y(x),type="l",ylim=c(-20,20))

plot(x,f(x),type='l',ylim=c(0,10))
