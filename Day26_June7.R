g <- function(x){x*(1-x)}
Dg <- function(x){1-2*x}
D2g <- function(x){0*x-2} #Is the zero important?

uniroot(Dg,c(-10,10))$root
cv <- uniroot(Dg,c(-10,10))$root
cv

D2g(1/2)
D2g(cv)

my_plot <- function(f,left_bound,right_bound,gap = (right_bound-left_bound)/100, mar = c(2.5,2.5,0.25,0.25), type = "l",... ){
  x <- seq(left_bound,right_bound,gap)
  par(mar=mar)
  plot(x,f(x),type = type,...)
}
my_lines <- function(f,left_bound,right_bound,gap = (right_bound-left_bound)/100, type = "l",... ){
  x <- seq(left_bound,right_bound,gap)
  lines(x,f(x),type = type,...)
}

par(mfrow=c(1,1))
my_plot(g,-2,2)
points(cv,g(cv))

a <- -2
b <- 2
my_plot(g,a,b)
my_lines(Dg,a,b,col = "red")
abline(h=0, lty = 2)
my_lines(D2g,a,b, col = "green")
abline(v=cv,col="blue",lty = 2)
points(cv,g(cv))
points(cv,Dg(cv),col="red")
points(cv,D2g(cv),col="green")
legend((a+b)/2, (g(a)+g(b))/2, legend=c("f", "f\'", "f\'\'"),
       col=c("black","red", "green"), lty=1, cex=0.8)



h <- function(x){x^3-x}
Dh <- function(x){3*x^2-1}
D2h <- function(x){6*x}

my_plot(h,-2,2)

my_plot(Dh,-2,2)
abline(h=0)

-sqrt(1/3)
sqrt(1/3)

cv <- uniroot(Dh,c(-10,10))$root
cv.1 <- uniroot(Dh,c(-10,0))$root
cv.2 <- uniroot(Dh,c(0,10))$root

cv.1
-sqrt(1/3)
cv.2
sqrt(1/3)

D2h(-sqrt(1/3))
D2h(cv.1)

D2h(sqrt(1/3))
D2h(cv.2)

h(-1)
h(-sqrt(1/3))
h(cv.1)
h(sqrt(1/3))
h(cv.2)
h(2)

my_plot(h,-1,2)
