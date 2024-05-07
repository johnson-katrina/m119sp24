?uniroot

h <- function(x){3*x-15}
uniroot(h,c(1,10))

uniroot(h,c(1,10))$root

h.new <- function(x){h(x)-4}
uniroot(h.new,c(1,10))$root
19/3

h.new2 <- function(x){h(x)-exp(-x+16)}
uniroot(h.new2,c(1,10))$root
uniroot(h.new2,c(-10,50))$root
