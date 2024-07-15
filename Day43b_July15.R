F <- function(x){
  out <- rep(-1, length(x))
  out[(x <= 0)] <- rep(0,length(x[x <= 0]))
  out[(0 < x) & (x < 2)] <- (3/2)*x[(0 < x) & (x < 2)] - (3/4)*x[(0 < x) & (x < 2)]^2 + (1/8)*x[(0 < x) & (x < 2)]^3
  out[(x >= 2)] <- rep(1,length(x[x >= 2]))
  return(out) }

x.val <- seq(-2,10,0.1)
par(mfrow=c(1,1),mar=c(2.5,2.5,0.25,0.25))
plot(x.val,F(x.val),type='l')