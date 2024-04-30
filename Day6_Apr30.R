f <- function(x){x^3}

x <- seq(-10,10,0.1)
par(mfrow=c(2,2))
plot(x,f(x),type='l',lty=2,col='gray',xlim=c(-5,5),ylim=c(-6,10))
lines(x,f(x+2))
abline(h=0,v=0,col='gray')

plot(x,f(x),type='l',lty=2,col='gray',xlim=c(-5,5),ylim=c(-6,10))
lines(x,f(x-2))
abline(h=0,v=0,col='gray')

plot(x,f(x),type='l',lty=2,col='gray',xlim=c(-5,5),ylim=c(-6,10))
lines(x,f(x)+2)
abline(h=0,v=0,col='gray')

plot(x,f(x),type='l',lty=2,col='gray',xlim=c(-5,5),ylim=c(-6,10))
lines(x,f(x)-2)
abline(h=0,v=0,col='gray')

