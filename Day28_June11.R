## Let's create a function to solve this system of equations. 
solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}
## Before moving forward, let's verify that the function works.
## The solution to $2x+3y=4$, $5x+6y=7$ is $(-1,2)$.  
## Does this function yield the same result?
solvesystem(2,3,4,5,6,7)


solvesystem(-3,-1,13,2,-3,17)



library(data4led)
bulb <- led_bulb(1,seed=123)
t <- bulb$hours
y <- bulb$percent_intensity

## Solve the dl1/da1 = 0
b <- sum((y-100)*t)
d <- sum(t^2)
b/d
## Second derivative Test (we see the second derivative at the CV is negative)
-d
f1 <- function(x, a1 = b/d){100 + a1*x}
x <- seq(0,5000,10)
plot(t,y)
lines(x,f1(x),type = "l") 


## Solve system of first partials equal to zero
c11 <- sum(t^2)
c12 <- sum(t^3)
b1 <- sum((y-100)*t)
c21 <- c12
c22 <- sum(t^4)
b2 <- sum((y-100)*t^2)
  
sol <- solvesystem(c11,c12,b1,c21,c22,b2)
sol
a1 <- sol[1]
a2 <- sol[2]

## Define the model f2, and and create the plot. 
f2 <- function(x, a1 = sol[1], a2 = sol[2]){100 + a1*x + a2*x^2}
x <- seq(0,5000,10)
plot(t,y)
lines(x,f2(x),type = "l") 