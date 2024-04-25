#R can complete basic calculations
#Here we calculate the square of -2.
(-2)^2

#Here we define a vector, a list of real numbers.
x<-c(-2,-1,0,1,2,3,4,5)
#Let's print x.
x

#We can use the arrow assignment in the other order.
c(-2,-1,0,1,2,3,4,5) -> x

#Because we named the vector,
    #we can square all 8 numbers quickly as by squaring the vector.
#R squares each element (or entry) of the vector separately.
x^2

#Here we save the list of squared values.
y <- x^2

#We can plot in R just like we do by hand...
    #Make a list of input values and corresponding output values.
    #Plot the (input, output) pairs.
    #Connect the points with a line to create the graph.
#The default in the plot() command is to plot a set of points.
plot(x,y)
#Use the parameter type in the plot() function to connect the points with a line.
plot(x,y,type='l')

#We can also write our own functions in R.
#Below we have defined the function f(x)=x^2.
f <- function(x){x^2}
#We can use the function f to find the outputs corresponding to each value of x and plot them.
plot(x,f(x),type='l')
#We could also separate this into 2 steps as follows.
#Step 1: Use f to in outputs.
y <- f(x)
#Step 2: Plot the input output pairs and connect them with a line.
plot(x,y,type='l')

#Below we plot the piecewise function from our Brain Gains.
#First we make a list of our inputs
    #Some inputs from the branch when x<1.
x1 <- c(-2,-1,0,1)
    #Some inputs form the branch when x>1.
    #Note: When x=1, we follows the bottom branch and f(1)=3.
x2 <- c(1,2,3,4,5)
#Now we define a function for each branch of the piecewise function.
g1 <- function(x){2*x}
g2 <- function(x){0*x + 3}
#Then we find the output values corresponding to each input.
g1(x1)
g2(x2)
#Finally, we plot the input output pairs and connect them with a line.
plot(c(x1,x2), c(g1(x1),g2(x2)), type='l')
    #Note: This plot does not show us that f(1)=3.
    #We could revise the previous code so the graph shows clearly f(1)=3.
plot(x1, g1(x1), type='l', xlim=c(-2,5), ylim=c(-5,5))
points(1,g1(1))
lines(x2,g2(x2))
points(1,g2(1),pch=16)
     #We could also add the x-axis and y-axis to the plot as follows.
plot(x1, g1(x1), type='l', xlim=c(-2,5), ylim=c(-5,5),ylab="outputs",xlab="inputs",main='A plot of a piecewise function.')
points(1,g1(1))
lines(x2,g2(x2))
points(1,g2(1),pch=16)
abline(h=0,v=0,col="gray")

#Q: What would happen if we didn't include the 0*x?
g2.v2 <- function(x){3}
g2.v2(x2)
    #We only get a 3 rather than of vector of 3's.
plot(c(x1,x2), c(g1(x1),g2.v2(x2)), type='l')
    #This gives us an error because our vector of inputs and our vector of outputs are different lengths.
    #R doesn't know what the input output pairs are supposed to be.


#A scatter plot is a collection of data points.
#This code creates a scatter plot of the weight and fuel efficiency of 32 cars.
par(mar=c(4,4,0.25,0.25))
x <- mtcars$wt
y <- mtcars$mpg
plot(x,y)