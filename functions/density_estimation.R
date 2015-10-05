## density estimation ## 
 
threshold <- quantile(res6$avg_rel_dens, prob=0.4)

rad <- sample[, 'rad_mean'] # [glu] patent radicality (variable to estimate density of)
d1 <- sample[, 'reg_avg_rel_dens'] > threshold
d0 <- sample[, 'reg_avg_rel_dens'] <= threshold
base.rate.d1 <- sum(d1) / (sum(d1) + sum(d0))

rad.density     <- density(rad)
rad.d0.density  <- density(rad[d0])
rad.d1.density  <- density(rad[d1])

approxfun(rad.d0.density$x, rad.d0.density$y) -> rad.d0.f
approxfun(rad.d1.density$x, rad.d1.density$y) -> rad.d1.f

p.d.given.rad <- function(rad, base.rate.d1){
  p1 <- rad.d1.f(rad) * base.rate.d1
  p0 <- rad.d0.f(rad) * (1-base.rate.d1)
  p1 / ( p0 + p1)
}

x <- seq(0,1,0.01)
y <- p.d.given.rad(x, base.rate.d1)
plot(x,y, typ='l', col="red", xlab="radicality", ylab ='esimated p(knowledge div|rad)' )

plot(density(rad[d0]), col='blue', xlab='rad', ylab='estimate p(rad), 
      p(rad|diverse region), p(rad|non-diverse region)', main=NA, ylim=c(0,9))
lines(density(rad[d1]), col='red')

rel_dens <- as.factor(sample$reg_avg_rel_dens)
x <- as.numeric(levels(rel_dens))
y <- seq(0,1,0.01)
z <- matrix(NA, length(x), length(y))
for(i in 1:length(x)){
  print(i)
  denfun <- approxfun(density(sample$rad_mean[sample$reg_avg_rel_dens == x[i]])) 
  z[i,] <- denfun(y)
}

data <- list(
  x = x,
  y = y,
  z = z,
  type = "surface")
layout <- list(
  title = "Diversity density over relatedness density",
  scene = list(bgcolor = "rgb(244, 244, 248)"))

response <- py$plotly(data,
                      kwargs = list(
                        layout = layout,
                        filename = "first test",
                        fileopt = "overwrite"))
