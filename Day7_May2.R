f <- function(x){ifelse(x < -2, -x - 4, ifelse(x <= 2, 0*x - 2, x - 4))}

x <- seq(-10,10,0.1)
y <- f(x)
plot(x,y, type='l',ylim=c(-4,8))
grid()


5-8
5-10
5-12
5-14
5-16
-3+-5+-7+-9+-11

n <- seq(4,8)
n
5-2*n
sum(5-2*n)

k <- seq(1,3)
prod(1+k^2)

plot(x, 3*x +7,type='l')
grid()
lines(x, (-7 - 5*x)/3)
points(-2,1,pch=16)

12-40


library(data4led)
dist <- led_time(2100)
hist(dist$percent_intensity,probability = TRUE)

head(dist)


f0 <- function(x,a=0,b=1){
  # Make sure a < b when using this function.
  ifelse(x < a,NaN, ifelse(x <= b, 1/(b-a), NaN))
}

a <- 98
b <- 102
x <- seq(a,b,0.1)
y <- f0(x,a,b)

par(mfrow=c(1,2),mar=c(2.5,2.5,1,0.25))
plot(x,y,type='l',xlim=c(90,110), ylim = c(0,1))
mtext('For f0: a=98, b=102', side = 3, line = 0)

a <- 100
x <- seq(a,b,0.1)
y <- f0(x,a,b)

plot(x,y,type='l',xlim=c(90,110), ylim = c(0,1))
mtext('change a=100 (keep b=102)', side = 3, line = 0)