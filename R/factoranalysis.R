# here are some functions for principal component method for
# factor analysis that I wrote for my homework.  

kval <- function (x) { # determine the number of factors
  x <- as.matrix(x)
  if(!isSymmetric(x))
    stop("Hey genius, x is not a symmetric matrix. 
         You look great though.")
  e <- eigen(x)$values # extract the eigen values from the matrix
  Cumm <- cumsum(e) / sum(e) # cumulative sum
  Prop <- e / sum(e) # porportion
  RESULT <- cbind(e, Cumm, Prop)
  colnames(RESULT) <- c("EigenVals", "Cummulative", "Proportion")
  return(list(k = which(RESULT[,2] >= .9)[1], data = RESULT))
}

fl <- function(x, k = NULL) { # determine the loadings
  if (is.null(k))
    k <- kval(x)$k
  x <- as.matrix(x)
  if(!isSymmetric(x))
    stop("Hey genius, x is not a symmetric matrix. 
         You look great though.")
  if (k == dim(x)[1])
    stop("You're a good person but this method will do you no good.
         Consider a different one.")
  v <- eigen(x)$vectors # get the eigen vectors
  if (k == 1){
    v <- as.matrix(v[, 1])
  } else {
    v <- v[, 1:k]
  }
  e <- eigen(x)$values[1:k]
  f <- matrix(ncol = k, nrow = dim(v)[1])
  for (i in 1:k) {
    f[, i] <- sqrt(e[i]) * v[, i]
  }
  colnames(f) <- paste0("factor", 1:k)
  rownames(f) <- colnames(x)
  return(f)   
}

fls <- function(x, ...) {
  loadings <- fl(x, ...)
  comm <- apply(apply(loadings, 2, function(x) x ^ 2), 1, sum)
  result <- cbind(loadings, communality = comm)
  return(list(model = result))
}

plot.loadings <- function(x, y, z) {
  dt <- data.frame(x, y, z)
  names(dt) <- c("x", "y", "z")
  gg <- ggplot(data = dt, aes(x = x, y = y)) + theme_bw()
  gg <- gg + geom_text_repel(aes(label = z), size = 2,
                             segment.alpha = 0.5,
                             segment.size = 0.25)
  gg <- gg + geom_point(color = "red", size = 3)
  gg <- gg + xlab("Factor1") + ylab("Factor2")
  return(gg)
}

