#Objective Function
A <- function(y){300*y - 2*y^2}

#Collection of Inputs
y <- seq(0,150,0.1)
#The Area
plot(y,A(y),type="l")
#It looks like the maximizer is y=75.
abline(v=75,col=2)
