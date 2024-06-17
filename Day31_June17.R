fxx <- 2
fxy <- 4
fxz <- 0
fyy <- 2
fyz <- 2
fzz <- 2

(D1 <- fxx)
(D2 <- det(matrix(c(fxx,fxy,fxy,fyy),2,2,byrow=TRUE)))
(D3 <- det(matrix(c(fxx,fxy,fxz, fxy,fyy,fyz, fxz,fyz,fzz),3,3,byrow=TRUE)))

rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data3_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)

a <- sum(y*exp(-x))/sum(exp(-x)^2)
a 

f <- function(x,a){a*exp(-x)}
plot(x,y)
t <- seq(min(x),max(x),0.1)
lines(t,f(t,a))