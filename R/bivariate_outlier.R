biv.out <- function(x, y, d = 7) {
  a <- cbind(x, y)
  p <- length(a[1,  ])
  param <- MVA::biweight(a[, 1:2])
  m1 <- param[1]
  m2 <- param[2]
  s1 <- param[3]
  s2 <- param[4]
  r <- param[5]
  x <- (a[, 1] - m1)/s1
  y <- (a[, 2] - m2)/s2
  e <- sqrt((x * x + y * y - 2 * r * x * y)/(1 - r * r))
  e2 <- e * e
  em <- median(e)
  emax <- max(e[e2 < d * em * em])
  r1 <- emax * sqrt((1 + r)/2)
  r2 <- emax * sqrt((1 - r)/2)
  theta <- ((2 * pi)/360) * seq(0, 360, 3)
  xpp <- m1 + (r1 * cos(theta) + r2 * sin(theta)) * s1
  ypp <- m2 + (r1 * cos(theta) - r2 * sin(theta)) * s2
  if(sum(sp::point.in.polygon(a[,1], a[,2], xpp, ypp) != 1) != 0)
    index <- sp::point.in.polygon(a[,1], a[,2], xpp, ypp) == 0
  return(index)
}