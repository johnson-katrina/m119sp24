f <- function(x){log(3*x,2) - 2}
uniroot(f,c(0,10))$root
16/3

f.shift <- function(x){f(x) -2}
uniroot(f.shift,c(0,10))$root

g <- function(x){
  3*x-15-exp(-x+6)
}

uniroot(g,c(0,5))$root

x <-seq(0,30,1)
plot(x,g(x), type="l")
abline(h=0, col = "lightgray")

g(0)
g(5)
g(20)

uniroot(g,c(0,20))$root

#The equation 1/x=0 has no solution.
f <- function(x){
  1/x
}
uniroot(f,c(-10,-3))$root
#Uniroot finds "a solution" ... 
uniroot(f,c(-1,1))$root
    #but this value for x does not actually make the equation true 
    #so it is not a solution
uniroot(f,c(-1,1))
