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


g <- function(x){2*x}
a <- 0
b <- 5
n <- 5
dx <- (b-a)/n
draw_rect_approx(g,a,b,n) 

xi <- seq(a+dx/2,b,dx)  #Center of each rectangle
Ai <- g(xi)*dx          #Area of each rectangle
data.frame(x_i = xi, A_i = Ai)

c(total_area =sum(Ai),  
  expected_value = sum(xi*Ai)/sum(Ai))

g <- function(x){2*x}
a <- 0
b <- 5
n <- 50
dx <- (b-a)/n
draw_rect_approx(g,a,b,n) 

xi <- seq(a+dx/2,b,dx)  #Center of each rectangle
Ai <- g(xi)*dx          #Area of each rectangle
data.frame(x_i = xi, A_i = Ai)

c(total_area =sum(Ai),  
  expected_value = sum(xi*Ai)/sum(Ai))


g <- function(x){x^2}
a <- 0
b <- 5
n <- 50
dx <- (b-a)/n
draw_rect_approx(g,a,b,n) 

xi <- seq(a+dx/2,b,dx)  #Center of each rectangle
Ai <- g(xi)*dx          #Area of each rectangle
data.frame(x_i = xi, A_i = Ai)

c(total_area =sum(Ai),  
  expected_value = sum(xi*Ai)/sum(Ai))