draw_rect_approx <- function(f,a,b,num_rectangles, method = "mid"){
  n <- num_rectangles
  dx <- (b-a)/n
  x <- c(a,seq(a,b,dx/100),b,a)
  y <- c(0,f(seq(a,b,dx/100)),0,0)
  par(mar=c(2.5,2.5,0.25,0.25))
  plot(x,y,type = "l")
  
  if(method == "left"){
    xi <- seq(a+0*dx/2,b-dx/2,dx)
    lines(xi,f(xi),type = "h")
    lines(xi,f(xi),type = "s")
    lines(c(xi[n],xi[n]+dx),f(c(xi[n],xi[n])),type = "l")
    lines(c(xi[n],xi[n]+dx),f(c(xi[n],xi[n])),type = "h")
  }
  else if(method == "right"){
    xi <- seq(a+dx,b+dx/2,dx)
    lines(xi-dx,f(xi),type = "h")
    lines(xi-dx,f(xi),type = "s")
    lines(c(xi[n]-dx,xi[n]),f(c(xi[n],xi[n])),type = "l")
    lines(c(xi[n]-dx,xi[n]),f(c(xi[n],xi[n])),type = "h")
  } 
  else{#Use midpoint
    xi <- seq(a+dx/2,b,dx)
    lines(xi-dx/2,f(xi),type = "h")
    lines(xi-dx/2,f(xi),type = "s")
    lines(c(xi[n]-dx/2,xi[n]+dx/2),f(c(xi[n],xi[n])),type = "l")
    lines(c(xi[n]-dx/2,xi[n]+dx/2),f(c(xi[n],xi[n])),type = "h")
  }
}


draw_rug <- function(f,a,b,num_points=100){
  x <- c(a,seq(a,b,(b-a)/num_points),b,a)
  y <- c(0,f(seq(a,b,(b-a)/num_points)),0,0)
  par(mar=c(2.5,2.5,0.25,0.25))
  plot(x,y,type = "l")
  polygon(x,y,col="gray")
}



f <- function(x){2 + 0*x}
draw_rug(f,0,6)

f0 <- function(x,a=0,b=1){(1/(b-a))+0*x}

x <- seq(-1,2,0.1)
y <- f0(x,-1,2)

plot(x,y,type='l',xlim=c(-2,3),ylim=c(0,0.5))

set.seed(123)
tmp <- runif(25000,-1,2)
length(tmp)

hist(tmp,probability=TRUE)
lines(x,y,col=3)

num <- which(tmp<1.2)
head(num)
head(tmp)
length(num)
prob <- length(num)/length(tmp)
prob

(1.2-(-1))*(1/3)



g <- function(x){4-x^2}
a <- -1
b <- 2
n <- 10
dx <- (b-a)/n
draw_rect_approx(g,a,b,n) 

#Start at half of dx to the right of a, and then step by dx.
xi <- seq(a+dx/2,b,dx)
points(xi,g(xi),pch=16,col=2)
segments(xi,rep(0,length(xi)),xi,g(xi),col=2)

Ai <- g(xi)*dx
sum(Ai)


draw_rect_approx(g,a,b,n,method='left') 
#Start a, and then step by dx.
xi <- seq(a,b-dx,dx)
points(xi,g(xi),pch=16,col=2)
segments(xi,rep(0,length(xi)),xi,g(xi),col=2)

Ai <- g(xi)*dx
sum(Ai)

draw_rect_approx(g,a,b,n,method='right') 
#Start a plus dx, and then step by dx.
xi <- seq(a+dx,b,dx)
points(xi,g(xi),pch=16,col=2)
segments(xi,rep(0,length(xi)),xi,g(xi),col=2)

Ai <- g(xi)*dx
sum(Ai)



g <- function(x){4-x^2}
a <- -1
b <- 2
n <- 10
dx <- (b-a)/n

#Mid: Start at half of dx to the right of a, and then step by dx.
xi.m <- seq(a+dx/2,b,dx)
#Left: Start a, and then step by dx.
xi.L <- seq(a,b-dx,dx)
#Start a plus dx, and then step by dx.
xi.R <- seq(a+dx,b,dx)

Ai.m <- g(xi.m)*dx
Ai.L <- g(xi.L)*dx
Ai.R <- g(xi.R)*dx
sum(Ai.m)
sum(Ai.L)
sum(Ai.R)

