source("penfit.R")
source("rp-plot4d.R")

#----------------------------------------------------------------------------

update.smst <- function(model, pen.fit.model) {
  try({ pen.fit.model <- cache.result(pen.fit.model, c("coef", "fitted", "residuals", "effective.df", "residual.df", "estimate.sd")) })
#  model$fitted <- fitted(pen.fit.model)
  model$alpha <- coef(pen.fit.model)
  model$lambda <- pen.fit.model$lambda
  model$df.model <- effective.df(pen.fit.model)
  model$df.residuals <- residual.df(pen.fit.model)
  model$pen.fit.model <- pen.fit.model
  model
}

#----------------------------------------------------------------------------

sm.st <- function(...) {
  return(sm.st.splines(...))
}

sm.st.splines <- function(x, t, y, lambda, df, ig.a=1e-4, ig.b=1e-4, prior.lambda, nseg = rep(7, 3), lambda.rel.time=1, lambda.rel=c(1,1,lambda.rel.time), pord=2, bdeg=3, driver="gsvd", computeD=FALSE) {

   
   X      <- cbind(x, t)
   xrange <- t(apply(X, 2, range))
   mat    <- st.matrices(X, xrange, ndims = 3, nseg = nseg, pord=pord, bdeg=bdeg, lambda.rel=lambda.rel,computeD=computeD)
   B      <- mat$B
   m      <- ncol(mat$B)
   P      <- mat$P

   
   if (missing(prior.lambda))
     prior.lambda <- function(lambda) 1
   
   pen.fit.model <- pen.fit(B=B, DtD=P, y=y, driver=driver, lambda=lambda, ig.a=ig.a, ig.b=ig.b, prior.lambda=prior.lambda)
   
   if (missing(lambda))
     pen.fit.model <- update.df(pen.fit.model, df)
   
   result <- list(m = m, B = B, y = y, X = X, xrange = xrange, nseg = nseg, pord=pord, bdeg=bdeg)

   if (computeD)
     result$D <- mat$D
   class(result) <- c("smst","smst.splines")
  
   result <- update.smst(result, pen.fit.model)

   invisible(result)
}

update.lambda.smst <- function(model, ...) {
  update.smst(model, update.lambda(model$pen.fit.model, ...))
}

update.df.smst <- function(model, ...) {
  update.smst(model, update.df(model$pen.fit.model, ...))
}

best.lambda.smst <- function(model, ...) {
  update.smst(model, best.lambda(model$pen.fit.model, ...))
}

profile.lambda.smst <- function(model, ...) {
  profile.lambda(model$pen.fit.model, ...)
}

average.smst <- function(model , ...) {
  update.smst(model, average(model$pen.fit.model, ...))
}

residuals.smst <- function(model, ...) {
  residuals(model$pen.fit.model, ...)
}

fitted.smst <- function(model, ...) {
  fitted(model$pen.fit.model, ...)
}

smst.run <- function(fun, pen.fit.model, lambda, ...) {
  if (missing(lambda))
    return(fun(pen.fit.model, ...))
  sapply(lambda, function(l) fun(pen.fit.model, l, ...)) 
}

effective.df.smst <- function(model, lambda, ...) {
  smst.run(effective.df, model$pen.fit.model, lambda, ...)
}

log.post.smst <- function(model, lambda, ...) {
  smst.run(log.post, model$pen.fit.model, lambda, ...)
}

AIC.smst <- function(model, lambda, ...) {
  smst.run(AIC, model$pen.fit.model, lambda, ...)
}

GCV.smst <- function(model, lambda, ...) {
  smst.run(GCV, model$pen.fit.model, lambda, ...)
}

predict.smst <- function(model, ...) {
  NextMethod("predict")
}

predict.smst.splines <- function(model, newx, newt, newB, ...) {
  if (!missing(newB)) {
    newdata <- newB
  } else {
    if (!missing(newx)) {
      if (!missing(newt))
        newx <- cbind(newx, newt)
      newdata <- st.matrices(newx, model$xrange, ndims = 3, nseg = model$nseg, bdeg=model$bdeg)$B
    } else {
      newdata <- model$B
    }
  }
  predict(model$pen.fit.model, newdata, ...)
}

predict.smst.gp <- function(model, newx, newt, ...) {
  if (!missing(newx)) {
    if (!missing(newt))
      newx <- cbind(newx, newt)
  } else {
    newx <- model$X
  }
  colnames(newx) <- c("X1","X2","X3")
  newC <- matrix(0, ncol=nrow(model$X), nrow=nrow(newx))
  for (j in 1:ncol(newx))
    newC <- newC+outer(newx[,j], model$X[,j], "-")^2
  newC <- sqrt(newC)
  newC <- cov.spatial(newC, model$cov.model, cov.pars=c(1, model$range), kappa=model$kappa)
  predict(model$pen.fit.model, newC, offset=predict(model$y.pre, as.data.frame(newx)), ...)
}

#----------------------------------------------------------------------------

plot.smst <- function(model, lambda, type="response", ngrid=20, show.data, panel="full", superimpose=NULL, superimpose.text, fn=I, ...) {
  panel.options <- c("full", "notkr", "norpanel", "mainplotonly")
  
  interactive <- pmatch(panel, panel.options) %in% c(1,2)
  if (interactive)
    require(rpanel)

  if (!missing(lambda))
    model <- update.lambda(model, lambda, ...)

  types <- c("response","se","se.prediction","confidence","prediction")
  type <- pmatch(type,types)
  if (is.na(type))
    stop("Invalid type argument.")
  type <- types[type]  
   
   if (missing(show.data))
    show.data <- !(type %in% c("se","se.prediction"))
 
   x    <- model$X
   ndim <- 3
   u    <- list(length = ndim)
   for (j in 1:ndim)
           u[[j]] <- seq(model$xrange[j, 1], model$xrange[j, 2], length = ngrid)

   if (!interactive) {
     time0 <- as.list(match.call(expand.dots = TRUE))$time0
     if (is.null(time0))
       time0 <- model$xrange[3, 1]
     u[[3]] <- time0
   }
  
   U <-  as.matrix(expand.grid(u))
  
   est  <- array(fn(predict(model, U, type=type)), dim = rep(ngrid, ndim))

   time <- model$X[,3]
   response <- fn(model$y)
   if (!show.data)
     response <- NULL

   rp.model <- list(x=cbind(u[[1]], u[[2]]), y=est, z=u[[3]])

   if (missing(superimpose.text))
     superimpose.text=bquote(lambda*"="*.(format(model$pen.fit.model$lambda))*" (df="*.(round(effective.df(model$pen.fit.model),2))*")")
  
   superimpose.fn <- superimpose
  
   simp <- function() {
     usr <- par()$usr
     title(superimpose.text)
     if (!is.null(superimpose.fn) & is.function(superimpose.fn))
       superimpose.fn()
   }
  
   rp.spacetime(space=model$X[,1:2], y=response, time=model$X[,3], model=rp.model, panel=panel, superimpose=simp, ...)
}

#----------------------------------------------------------------------------

st.matrices <- function(x, xrange, ndims, nseg, bdeg = 3, pord = 2, lambda.rel=rep(1,3),
                            computeP = TRUE,  computeD=FALSE, sparsePenalty = FALSE) {

    # Compute a set of basis functions and a penalty matrix associated with x.
    # An intercept term and the main effect of any interaction terms are removed


    if (sparsePenalty)
      require(Matrix)

    n    <- nrow(x)
    if (missing(nseg)) nseg <- rep(7, 3)
    
    # Compute B-spline basis
    
    b <- list(length = 3)
    m <- vector(length = 3)
    for (i in 1:3) {
       b[[i]] <- bbase(x[,i], xl = xrange[i , 1], xr = xrange[i, 2], nseg = nseg[i], 
                       deg = bdeg)
       m[i]   <- ncol(b[[i]])
    }

    B <- b[[1]]
    B <- t(apply(cbind(b[[1]], b[[2]]), 1,
                            function(x) c(x[1:m[1]] %x% x[-(1:m[1])])))
    B <- t(apply(cbind(B,  b[[3]]), 1, 
                function(x) c(x[1:(m[1]*m[2])] %x% x[-(1:(m[1]*m[2]))])))
    
     result <- list(B = B, xrange = xrange, nseg = nseg, bdeg = bdeg, pord = pord)

    if (computeP || computeD) {
      # Construct smoothness penalty matrices
      D <- list(length = 3)
      for (i in 1:3) {
        D[[i]] <- sqrt(lambda.rel[i]) * diff((if (sparsePenalty) Diagonal else diag)(m[i]), diff = pord)
      }
      if (computeP)
        result$P <- crossprod(D[[1]]) %x% diag(m[2]*m[3]) +
                    diag(m[1]) %x% crossprod(D[[2]]) %x% diag(m[3]) + 
                    diag(m[1]*m[2]) %x% crossprod(D[[3]])
      if (computeD)
        result$D <- (if (sparsePenalty) rBind else rbind)(
                       D[[1]] %x% diag(m[2]*m[3]) ,
                       diag(m[1]) %x% D[[2]] %x% diag(m[3]) ,
                       diag(m[1]*m[2]) %x%D[[3]])      
    }
    invisible(result)
}

bbase <- function(x, xl = min(x), xr = max(x), nseg = 10, deg = 3) {
  # Construct B-spline basis
  dx <- (xr - xl) / nseg
  knots <- seq(xl - deg * dx, xr + deg * dx, len=nseg+1+deg*2)
  P <- outer(x, knots, tpower, deg)
  n <- dim(P)[2]
  D <- diff(diag(n), diff = deg + 1) / (gamma(deg + 1) * dx ^ deg)
  B <- (-1) ^ (deg + 1) * P %*% t(D)
  B
}


tpower <- function(x, t, p)
# Truncated p-th power function
    (x - t) ^ p * (x > t)

#----------------------------------------------------------------------------

tune.lambda.rel <- function(lambda.rel, lambda=log.post, ...) {
  lambda.rel <- as.matrix(lambda.rel)
  if (ncol(lambda.rel)==1)
    lambda.rel <- t(lambda.rel)
  log.post <- numeric(nrow(lambda.rel))
  prior.part <- numeric(nrow(lambda.rel))
  for (i in 1:length(log.post)) {
    model <- sm.st(..., lambda.rel=lambda.rel[i,],computeD=TRUE, lambda=lambda)
    Rdiag <- diag(qr.R(qr(model$D)))
    prior.part[i] <- sum(log(Rdiag[Rdiag>sqrt(.Machine$double.eps)]))
    log.post[i] <- log.post(model) + prior.part[i]
    print(model$pen.fit.model$lambda)
  }
  
  list(lambda.rel=lambda.rel, prior.part=prior.part, log.post=log.post, best.lambda.rel=lambda.rel[which.max(log.post),])
}
                                            
