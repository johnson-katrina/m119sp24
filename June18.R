rm(list=ls())

x <- c(1,2,3,4,5,6,7)
y <- c(24.1,29.5,34.5,39.1,46.6,51.9,59.1)

m.best <- (sum(x*y) - length(x)*mean(x)*mean(y))/(sum(x^2) - length(x)*(mean(x))^2)
b.best <- mean(y) - m.best*mean(x)

m.best
b.best

h <- function(x, b=0, m=1){
  b + m*x
}

x.val <- seq(min(x),max(x),(max(x)-min(x))/5) 
y.val <- h(x.val,b=b.best, m=m.best)

par(mfrow=c(1,1),mar=c(2.5,2.5,0.25,0.25))
plot(x,y,type='p',pch=16)
lines(x.val,y.val,col=3)