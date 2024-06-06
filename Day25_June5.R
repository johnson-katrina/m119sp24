v <- function(x){0.25*x*(6-x^2)}

x <- seq(0,6,0.1)
plot(x,v(x),type='l')
plot(x,v(x),type='l',ylim=c(-5,2))
abline(v=1.5,col=2)
