n <- seq(1,44)
b11 <- sum(n)
b12 <- 3*44
b21 <- 5*44
b22 <- b11
c1 <- 7*44
c2 <- sum(n^2)

x <- (c1*b22 - b12*c2)/(b11*b22 - b12*b21)
y <- (b11*c2 - c1*b21)/(b11*b22 - b12*b21)

f1 <- function(x,b1,b2,c){(c-b1*x)/b2}

x_in <- seq(x-10,x+10,0.1)
plot(x_in,f1(x_in,b11,b12,c1),type="l")
lines(x_in,f1(x_in,b21,b22,c2),col=3)
points(x,y,pch=16)
