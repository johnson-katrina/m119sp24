data <- read.csv(url("https://byuistats.github.io/M119/logLikelihood_practice.csv"))

x <- data$x
y <- data$y2
length(x)

plot(x,y,pch=16)

c11 <- 50
c12 <- sum(x)
c21 <- c12
c22 <- sum(x^2)
b1 <- sum(y)
b2 <- sum(x*y)

## Create a function to solve a system of equations. 
solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}

sol <- solvesystem(c11, c12, b1, c21, c22, b2)
best_b <- sol[1] 
best_m <- sol[2] 

best_b
best_m

fxx <- -c11 
fxy <- -c12 
fyy <- -c22
D <- fxx*fyy - fxy^2
D
fxx

#Does it matter if b is the "first" variable or m is? No!
c11.check <- sum(x)
c12.check <- 50
c21.check <- sum(x^2)
c22.check <- c11
b1.check <- sum(y)
b2.check <- sum(x*y)

sol.check <- solvesystem(c11.check, c12.check, b1.check, c21.check, c22.check, b2.check)
best_b.check <- sol.check[2] 
best_m.check <- sol.check[1] 
best_b.check
best_m.check

fxx.check <- -c21.check 
fxy.check <- -c11.check 
fyy.check <- -c12.check
D.check <- fxx.check*fyy.check - fxy.check^2
D.check #notice D is the same
fxx.check #notice the value of fxx is different but the sign is the same
  #since m is the "first" variable then 
  #fxx is the 2nd partial of the likelihood with respect to m (rather than with repect to b)

h <- function(x, b = best_b, m = best_m){b + m*x}

x_in <- seq(min(x),max(x),0.01)
plot(x,y,type='p',pch=16)
lines(x_in,h(x_in),col=3)
abline(h=0,lty=3,col='gray')
abline(v=0,lty=3,col='gray')