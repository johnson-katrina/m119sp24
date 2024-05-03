f <- function(x){3^(-x)}

x <- seq(-2,2,0.1)
plot(x,f(x),type='l')
grid()

exp(2)
exp(1)


x <- seq(-4,20,0.1)
y <- 10-sqrt(x+4)
par(mar=c(2.5,2.5,0.5,0.5))
plot(x,y,type='l')
grid()


x <- seq(-4,200,0.1)
y <- 10-sqrt(x+4)
par(mar=c(2.5,2.5,0.5,0.5))
plot(x,y,type='l')
grid()

pnt <- function(x){10-sqrt(x+4)}
pnt(0)
pnt(-4)
pnt(21)
pnt(117)
