#Here we define a function for a general line by using parameters a0 and a1.
f1 <- function(x,a1=3,a0=0){a1*x + a0}

#We plot f(x)=3x by defining a vector of inputs, calculating the corresponding outputs,
    #and then plotting the input output pairs and connecting them with a line.
x1 <- seq(-4,7,0.2)
y1 <- f1(x1)
plot(x1,y1,type='l')

#We plot f(x)=-2 by defining a vector of inputs, calculating the corresponding outputs,
    #and then plotting the input output pairs and connecting them with a line.
x2 <- seq(-5,5)
#We specify the parameter values a1=0 and a0=-2 because they 
    #are different from the default values in the function defined in line 1.
y2 <- f1(x2,a1=0,a0=-2)
plot(x2,y2,type='l')

#Use these commands to get the working directory or set the working directory.
    #You can also use the Session menu to adjust the working directory.
getwd()
setwd("/Users/katjohnson/Documents/GitHub/m119sp24")