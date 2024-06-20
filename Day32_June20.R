rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data5_ls.csv"))
x <- data$x
y <- data$y

length(x)

a <- sum(y*log(x))/sum((log(x))^2)

f <- function(x,a=1){a*log(x)}
t <- seq(min(x),max(x),0.1)
plot(x,y,pch=16)
lines(t,f(t,a),col=2)

f(4,a)
f.shift <- function(x,a){f(x,a)-5}
x_sol <- uniroot(f.shift,c(1,5),a=a)$root
x_sol

points(x_sol,f(x_sol,a),col=2,pch=16)
abline(h=5,col="gray",lty=3)
abline(v=x_sol,col="gray",lty=3)

points(4,f(4,a),col=3,pch=16)
abline(h=f(4,a),col="gray",lty=3)
abline(v=4,col="gray",lty=3)