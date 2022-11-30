##########################################################################
# GENERIC METHODS

# pen.fit sets up a pen.fit.xxx object which is aimed at solving
# || y-B%*%beta ||^2 + lambda * || D%*%beta ||^2
# The function doesn't do much itself. Its main job is to call the
# initialisation method of the corresponding "driver" class.

# For kernel B is the n x n kernel/covariance matrix 

pen.fit <- function(B=NULL, D=NULL, DtD=NULL, y, driver="gev", rank.D=NULL, lambda, ig.a, ig.b, prior.lambda, ...) {
  # Fill in missing parameters
  if (!missing(prior.lambda))
    prior.lambda <- prior.lambda
  # Handle call to model averaging
  if (!missing(lambda) && is.character(lambda) && (lambda=="avg"))
    return(pen.fit.avg(B=B, D=D, DtD=DtD, y=y, driver=driver, rank.D=rank.D, ig.a=ig.a, ig.b=ig.b, prior.lambda=prior.lambda))
  # Match the driver argument
  drivers <- c("gev","gsvd","gsvd2","qr","choleski","kernel")
  driver <- drivers[pmatch(driver, drivers)]
  # Ensure that either D or DtD is present
  if (driver!="kernel" && is.null("B"))
    stop("You have to specify B or use driver='kernel'")
  if (is.null(D) && is.null(DtD))
    stop("You have to specify either D or DtD")
  # Store the arguments (requires less coding in the specific drivers)
  object <- list(B=B, D=D, DtD=DtD, y=y, cache=list())
  if (!missing(ig.a))
    object$ig.a <- ig.a
  if (!missing(ig.b))
    object$ig.b <- ig.b
  if (!missing(prior.lambda))
    object$prior.lambda <- prior.lambda
  class(object) <- c("pen.fit", paste("pen.fit",driver,sep="."))
  # Obtain the correspinding "constructor" function and call it
  object <- initialise.fit(object)
  # Check whether the rank of D has been determined (otherwise set it)
  if (!missing(rank.D) && !is.null(rank.D)) {
    object$rank.D <- rank.D
  } else {
    if (is.null(object$rank.D))
      if (!is.null(object$D)) {
        object$rank.D <- qr(D)$rank
      } else {
        object$rank.D <- qr(DtD)$rank
      }
  }
  # Finally set lambda if specified
  if (!missing(lambda)) 
    object <- update.lambda(object, lambda)
  object
}

# The function inititalises the fitting procedure (NOT TO BE CALLED BY THE USER)
initialise.fit <- function(object, ...) {
  UseMethod("initialise.fit")
}

initialise.fit.pen.fit <- function(object, ...) {
  NextMethod("initialise.fit")
}

# The function update lambda sets the current value of lambda
update.lambda <- function(object, ...) {
  UseMethod("update.lambda")
}

update.lambda.pen.fit <- function(object, lambda, cache=list(), ...) {
  # Handle call for best.lambda if necessary
  if (is.function(lambda) || is.character(lambda))
    return(best.lambda(object, lambda, ...))
  # Check whether the value of lambda has really changed
  if (is.null(object$lambda) || object$lambda!=lambda) {
    # If so let the driver-class function do the work ...
    object$lambda <- lambda
    object$cache <- cache
    NextMethod("update.lambda")
  } else {
    object
  }
}

# This function computes the coefficient vector - is already a class method in R
coef.pen.fit <- function(object, lambda, ...) {
  # If lambda is provided, update the value of lambda
  if (!missing(lambda))
    object <- update.lambda(object, lambda, ...)
  if (is.null(object$lambda))
    stop("Set a value of lambda first (use update.lambda)")
  # Check whether we have cached the coefficients
  if (!is.null(object$coef))
    return(object$coef)  
  # The effective df will be computed by the driver-class function
  NextMethod("coef")
}

# This function returns the fitted values
fitted.pen.fit <- function(object, lambda, ...) {
  # Set lambda (if neccessary)
  if (!missing(lambda))
    object <- update.lambda(object, lambda, ...)
  if (is.null(object$lambda))
    stop("Set a value of lambda first (use update.lambda)")
  # Try to run the fitted method of the specific diver class
  result <- try(NextMethod("fitted"))
  if (!inherits(result, "try-error") & !is.null(result))
    return(result)
  # Check whether we have cached the fitted values
  if (!is.null(object$cache$fitted))
    return(object$cache$fitted)  
  # If none exists, perform calculations manually
  object$B%*%coef(object, ...)
}

# This function returns the fitted values
residuals.pen.fit <- function(object, lambda, ...) {
  # Set lambda (if neccessary)
  if (!missing(lambda))
    object <- update.lambda(object, lambda, ...)
  if (is.null(object$lambda))
    stop("Set a value of lambda first (use update.lambda)")
  # Try to run the fitted method of the specific diver class
  result <- try(NextMethod("residuals"))
  if (!inherits(result, "try-error") & !is.null(result))
    return(result)
  # Check whether we have cached the residuals
  if (!is.null(object$cache$residuals))
    return(object$cache$residuals)  
  # If none exists, perform calculations manually
  object$y - fitted(object,  ...)
}

vars.unscaled.pred <- function(object, newdata) {
  UseMethod("vars.unscaled.pred")
}

vars.unscaled.pred.pen.fit <- function(object, newdata) {
  NextMethod("vars.unscaled.pred")
}

# This function predicts for new data
# type can be response, se, confidence, prediction
predict.pen.fit <- function(object, newdata, type="response", lambda, level=0.95, offset=0, ...) {
  # Check for newdata
  if (missing(newdata)) {
    if (is.null(object$B)) {
      stop("Training data not retained. You need to specify newdata.")
    } else {
      newdata <- object$B
    }
  }
  # Set lambda (if neccessary)
  if (!missing(lambda))
    object <- update.lambda(object, lambda, ...)
  if (is.null(object$lambda))
    stop("Set a value of lambda first (use update.lambda)")
  # Match the type
  types <- c("design","response","se","se.prediction","confidence","prediction")
  type <- pmatch(type,types)
  if (is.na(type))
    stop("Invalid type argument.")
  type <- types[type]
  if (type=="design") {
    return(newdata)
  }    
  if (type=="response") {
    return(offset+drop(newdata%*%coef(object)))
  }
  if (type %in% c("se","se.prediction")) {
   v <- vars.unscaled.pred.pen.fit(object, newdata)
    if (type=="se.prediction")
      v <- v+1
    return(estimate.sd(object)*sqrt(v))
  }
  # Handle confidence and prediction intervals
  if (type %in%  c("confidence","prediction")) {
    warning("Intervals are non-/semi-Bayesian")
    response <- predict(object, newdata, type="response", ...)
    if (type=="prediction") {
      se <- predict(object, newdata, type="se.prediction", ...)
    } else { 
      se <- predict(object, newdata, type="se", ...)
    }
    return(response + qt(1-(1-level)/2, df=residual.df(object))* cbind(lower=-se,upper=se))
  }
}

# This function computes the effective degrees of freedom
effective.df <- function(object, ...) {
  UseMethod("effective.df")
}


effective.df.pen.fit <- function(object, lambda, ...) {
  # If lambda is provided, update the value of lambda
  if (!missing(lambda))
    object <- update.lambda(object, lambda, ...)
  if (is.null(object$lambda))
    stop("Set a value of lambda first (use update.lambda)")
  # Check whether we have cached the effective degree of freedom
  if (!is.null(object$cache$effective.df))
    return(object$cache$effective.df)
  # The effective df will be computed by the driver-class function
  NextMethod("effective.df")
}

# This function computes the effective residual degrees of freedom
residual.df <- function(object, ...) {
  UseMethod("residual.df")
}

residual.df.pen.fit <- function(object, lambda, ...) {
  # If lambda is provided, update the value of lambda
  if (!missing(lambda))
    object <- update.lambda(object, lambda, ...)
  if (is.null(object$lambda))
    stop("Set a value of lambda first (use update.lambda)")
  # Check whether we have cached the effective degree of freedom
  if (!is.null(object$cache$residual.df))
    return(object$cache$residual.df)
  # The effective residual df will be computed by the driver-class function
  NextMethod("residual.df")
}

# Return the residual sum of squares
RSS <- function(object, ...) {
  UseMethod("RSS")
}

RSS.pen.fit <- function(object, ...) {
  return(sum(residuals(object, ...)^2))
}

# Return the value of the penalty
penalty <- function(object, ...) {
  UseMethod("penalty")
}

penalty.pen.fit <- function(object, ...) {
#  result <- try(NextMethod("penalty"))
#  if (!inherits(result, "try-error") & !is.null(result))
#    return(result)
  coef <- coef(object, ...)
  if (!is.null(object$D))
    return((object$D%*%coef)^2)
  return(sum(coef*(object$DtD%*%coef)))
}

# Plot the L curve
L.curve <- function(object, ...) {
  UseMethod("L.curve")
}

L.curve.pen.fit <- function(object, from, to, n.steps=30, expand=1e-1, ...) {
  criteria <- c(GCV, AIC, AICc, BIC, log.post)
  criteria.names <- c("GCV","AIC","AICc", "BIC", "MAP")
  best <- list()
  lambdas <- c()
  for (i in 1:length(criteria)) {   
    object <- best.lambda(object, criteria[i][[1]])    
    object <- cache.result(object,"coef")
    best[[criteria.names[i]]] <- list(lambda=object$lambda, penalty = penalty(object), rss=RSS(object))
    lambdas <- c(lambdas,object$lambda)
  }
  if (missing(from))
    from <- exp((1+expand)*min(log(lambdas))-expand*max(log(lambdas)))
  if (missing(to))
    to <- exp(-expand*min(log(lambdas))+(1+expand)*max(log(lambdas)))
  lambdas <- exp(seq(log(from), log(to), length.out=n.steps))
  plot(matrix(unlist(lapply(lambdas, function(l) {  object <- update.lambda(object, lambda=l); object <- cache.result(object,"coef"); c(  RSS(object),  penalty(object))})), byrow=TRUE, ncol=2),type="l",log="xy",xlab="RSS",ylab="Penalty")
  col <- 1
  for (criterion in names(best)) {    
    col <- col+1
    points(best[[criterion]]$rss, best[[criterion]]$penalty, col=col)
  }
  legend("bottomleft",pch=1,col=2:col, names(best))
}



# Estimate standard deviation
estimate.sd <- function(object, ...) {
  UseMethod("estimate.sd")
}

estimate.sd.pen.fit <- function(object, lambda, ...) {
  # If lambda is provided, update the value of lambda
  if (!missing(lambda))
    object <- update.lambda(object, lambda, ...)
  if (is.null(object$lambda))
    stop("Set a value of lambda first (use update.lambda)")
  if (!is.null(object$cache$estimate.sd))
    return(object$cache$estimate.sd)
  if (is.null(object$ig.a) || is.null(object$ig.b)) {
    warning("Frequentist estimate of standard error used")
    return(sqrt(sum(residuals(object)^2) / (length(object$y) - effective.df(object))))
  }
  sqrt((object$ig.b+sum(object$y*residuals(object))/2)/(object$ig.a+length(object$y)/2+1)) 
}


# This function computes the logarithm of the posterior covariance
log.post.det <- function(object, ...) {
  UseMethod("log.post.det")
}

log.post.det.pen.fit <- function(object, lambda, ...) {
  # If lambda is provided, update the value of lambda
  if (!missing(lambda))
    object <- update.lambda(object, lambda, ...)
  if (is.null(object$lambda))
    stop("Set a value of lambda first (use update.lambda)")
  # The effective df will be computed by the driver-class function
  NextMethod("log.post.det")
}

# This function returns the log-posterior
log.post <- function(object, ...) {
  UseMethod("log.post")
}

log.post.pen.fit <- function(object, lambda, ig.a, ig.b, prior.lambda, ...) {
  if (!missing(lambda))
    object <- update.lambda(object, lambda)
  if (is.null(object$lambda))
    stop("Set a value of lambda first (use update.lambda).")
  if (!missing(ig.a) & !missing(ig.b)) {
    object$ig.a <- ig.a
    object$ig.b <- ig.b
  }
  if (!missing(prior.lambda))
    object$prior.lambda <- prior.lambda
  if (is.null(object$prior.lambda)) {
    object$prior.lambda <- function(lambda) 1
    warning("No prior.lambda specified. An improper flat prior is used.")
  }
  if (is.null(object$ig.a) | is.null(object$ig.b))
    stop("Set ig.a and ig.b either as arguments to this function or when creating the pen.fit object.")
  result <- 0.5 * object$rank.D * log(object$lambda) + 0.5 * log.post.det(object) - (object$ig.a+length(object$y)/2) * log(2*object$ig.b + sum(object$y*residuals(object))) + log(object$prior.lambda(object$lambda))
  attr(result, "maximize") <- TRUE
  attr(result, "criterion") <- "log(posterior)"
  result
}

# This function returns the AIC and the AICc
AIC.pen.fit <- function(object, lambda, AICc=FALSE, k=2, ...) {
  if (!missing(lambda))
    object <- update.lambda(object, lambda, ...)
  if (is.null(object$lambda))
    stop("Set a value of lambda first (use update.lambda).")
  sse <- sum(resid(object)^2)
  p <- effective.df(object)+1
  n <- nrow(object$B)
  result <-  n*log(sse / n) + k*p
  if (AICc)
    result <- result + k*p*(p+1) / (n-p-1)
  attr(result, "maximize") <- FALSE
  attr(result, "criterion") <- "AIC"
  if (AICc)
    attr(result, "criterion") <- "AICc"
  if (k!=2)
    attr(result, "criterion") <- paste(attr(result, "criterion")," with k =",k) 
  result
}
                
# This function computes the AICc
AICc <- function(object, ...) {
  UseMethod("AICc")
}

AICc.pen.fit <- function(object, ...) {
  AIC(object, AICc=TRUE, ... )
}

# This function computes the BIC
BIC.pen.fit <- function(object, ...) {
  AIC(object, k=log(length(object$y)), ... )
}
                              
# This function returns the GCV
GCV <- function(object, ...) {
  UseMethod("GCV")
}

GCV.pen.fit <- function(object, lambda,  ...) {
  if (!missing(lambda))
    object <- update.lambda(object, lambda, ...)
  if (is.null(object$lambda))
    stop("Set a value of lambda first (use update.lambda).")
  sse <- sum(resid(object)^2)
  p <- effective.df(object)
  n <- length(object$y)
  result <- 1/n * sse / (1 - p/n ) ^2
  attr(result, "maximize") <- FALSE
  attr(result, "criterion") <- "GCV"
  result
}

# This function sets lambda based on the effective degrees of freedom
update.df <- function(object, ...) {
  UseMethod("update.df")
}

update.df.pen.fit <- function(object, df, lambda.range=c(sqrt(.Machine$double.xmin), sqrt(.Machine$double.xmax)), ...) {
  if (is.function(df) || is.character(df))
    return(best.lambda(object, df, ...))  
  # If we already have the correct df return the object without doing anything
  if (!is.null(object$effective.df) && (object$effective.df==df))
    return(object)
  # Finding an interval of lambdas that contain the desired df
  lambda <- c(1,2)
  dfs <- c(effective.df(object, lambda[1]), effective.df(object, lambda[2]))
  lambda.factor <- 2
  repeat {
    if (dfs[1]<df) {
      if (lambda[1]<lambda.range[1])
        stop("Such a high df cannot be obtained.")
      lambda[2] <- lambda [1]
      dfs[2] <- dfs[1]      
      lambda[1] <- lambda[1] / lambda.factor   
      dfs[1] <- effective.df(object, lambda[1])
    } else {
      if (dfs[2]>df) {
        if (lambda[2]>lambda.range[2])
          stop("Such a low df cannot be obtained.")
        lambda[1] <- lambda[2]
        dfs[1] <- dfs[2]
        lambda[2] <- lambda[2] * lambda.factor
        dfs[2] <- effective.df(object, lambda[2])
      } else {
        break
      }
    }
    lambda.factor <- min(lambda.factor*2,2^10)
  }
  return(update.lambda(object, exp(uniroot(function(log.lambda) effective.df(object, exp(log.lambda))-df, log(lambda))$root)))
}

# Finds the best lambda accordinf to a fitting criterion
best.lambda <- function(object, criterion, ...) {
  if (is.character(criterion)) {
    criterion <- get(criterion, mode="function", envir=parent.frame())
    return(best.lambda(object, criterion, ...))
  }
  UseMethod("best.lambda")
}

best.lambda.pen.fit <- function(object, criterion, maximum, lambda, suppress.warnings=TRUE, initial.grid.size=10, initial.grid.factor=10, ...) {
 fn <- function(log.lambda, object, criterion, maximum, ...) {
    fn <- criterion(object, lambda=exp(log.lambda), ...)
    if (is.null(maximum)) {
      maximum <- attr(fn,"maximize")
      if (is.null(maximum))
        stop("You have to specify whether function should be minimized or maximized.")
    }
    (1-2*maximum)*fn
  }
  if (missing(maximum))
    maximum <- NULL
  if (suppress.warnings) {
    shut.up <- function(...) suppressWarnings(...)
  } else {
    shut.up <- function(...) { }
  }
  initial.value <- 0
  if (!is.null(object$lambda))
    initial.value <- log(object$lambda)
  if (!missing(lambda))
    initial.value <- log(lambda)
  if (initial.grid.size>0) {
    log.lambdas <- initial.value + log(initial.grid.factor) * seq(-(initial.grid.size-1)/2,(initial.grid.size-1)/2)
    initial.result <- sapply(log.lambdas, fn, object, criterion, maximum, ...)
    initial.value <- log.lambdas[which.min(initial.result)]
  }
  shut.up(best <- nlm(fn, initial.value, object, criterion, maximum, hessian=TRUE, ...))
  result <- update.lambda(object, exp(best$estimate))
  result$crit.hessian.logscale <- drop(best$hessian)
  result
}

# Create a profile plot of the fitting criterion
profile.lambda <- function(object, criterion, ...) {
  if (is.character(criterion)) {
    criterion <- get(criterion, mode="function", envir=parent.frame())
    return(profile.lambda(object, criterion, ...))
  }
  UseMethod("profile.lambda")
}

get.df.list <- function(object, from, to, steps) {
 from.df <- floor(effective.df(object, lambda=from))
 to.df <- ceiling(effective.df(object, lambda=to))
 if (from.df<to.df)
   return(c())
 steps <- min(steps, from.df-to.df+1)
 dfs <- rev(unique(round(exp(seq(log(to.df), log(from.df), length.out=steps)))))
 list(dfs=dfs, lambdas=sapply(dfs, function(df, object) update.df(object,df)$lambda, object))
}

profile.lambda.pen.fit <- function(object, criterion, from, to, n.steps=20, expand.factor=1e2, expand.relative=TRUE, show.df = 5, show.best=TRUE, skip.plot=FALSE, plot.args=list(), ...) {
  best <- NULL
  if (missing(from) || missing(to)) {
    object <- best.lambda(object, criterion, ... )
    best <- object$lambda
    if (expand.relative)
      expand.factor <- expand.factor*exp(sqrt(abs(object$crit.hessian.logscale)))
    from <- best/expand.factor
    to <- best*expand.factor
  }
  lambdas <- exp(seq(log(from), log(to), length.out=n.steps))
  fn <- function(lambda, object, criterion, ...) {
    criterion(object, lambda=lambda, ...)
  }
  profile <- lapply(lambdas, fn, object, criterion, ...)
  if (!skip.plot) {
    plot.args.default <- list(log="x", xlab="lambda", ylab="criterion", type="b")
    if (!is.null(attr(profile[[1]], "criterion")))
      plot.args.default$ylab <- attr(profile[[1]], "criterion")
    for (name in names(plot.args.default))
      if (!(name %in% names(plot.args)))
        plot.args[[name]] <- plot.args.default[[name]]
    profile <- unlist(profile)
    plot.args <- c(list(x=lambdas, y=profile), plot.args)
    do.call("plot", plot.args)
    if (!show.best)
      best <- NULL
    if (!is.null(best)) {
      abline(v=best, lty=3)
      axis(1, tick=FALSE, at=best, label=paste("lambda =",format(best)), line=-2, col="blue")
    if (show.df>0)
      axis(3, tick=FALSE, at=best, label=paste("df =",format(effective.df(object, best))), line=-2)
      
    }
    if (show.df>0) {
      df.list <- get.df.list(object, from, to, show.df)
      axis(3, at=df.list$lambdas, labels=df.list$dfs)
      mtext("effective degrees of freedom",3,2.5)
    }
  }
 invisible(list(lambdas=lambdas, profile=profile))
}

# This function performs model averaging

average <- function(object, ...) {
  UseMethod("average")
}

average.pen.fit <- function(object, n.points=20, ...) {
  pen.fit.avg(object=object, n.points=n.points, ...)
}

# Caches a number of pre-computed results
cache.result <- function(object, ...) {
  UseMethod("cache.result")
}

cache.result.pen.fit <- function(object, what, lambda, ...) {
  if (!missing(lambda))
    object <- update.lambda(object, lambda)
  lst <-as.list(match.call(expand.dots = TRUE))
  lst[[1]] <- NULL
  lst$what <- NULL
  lst$lambda <- NULL
  for (name in what) {
    func <- get(name, mode="function")
    result <- eval(as.call(c(func, lst)), envir=parent.frame())
    result.name <- attr(result,"slot")
    if (!is.null(result.name))
      name <- result.name
    object$cache[[name]] <- result
  }
  object
}

#########################################################################
# CHOLESKI-DECOMPOSITION-BASED DRIVER CLASS

initialise.fit.pen.fit.choleski <- function(object, ...) {
  # This method can't compute much ahead of time,
  # as it requires lambda for everything.
  object$Bty <- t(object$B)%*%object$y
  object$BtB <- crossprod(object$B)
  if (is.null(object$DtD))
    object$DtD <- crossprod(object$D)
  object
}

update.lambda.pen.fit.choleski <- function(object, lambda, ...) {
  object$chol <- chol(object$BtB + lambda*object$DtD)
  object$cov.unscaled <- chol2inv(object$chol)
  object
}

coef.pen.fit.choleski <- function(object, ...) {
  drop(backsolve(object$chol,forwardsolve(t(object$chol),object$Bty)))
}

effective.df.pen.fit.choleski <- function(object, ...) {
  # we exploit tr(AB) sum(Aij*Bji)
  sum(object$cov.unscaled*t(object$BtB))
}

residual.df.pen.fit.choleski <- function(object, ...) {
  mat <- object$cov.unscaled%*%object$BtB
  nrow(object$B) - 2 * sum(diag(mat)) + sum(mat*t(mat))
}

log.post.det.pen.fit.choleski <- function(object, ...) {
  -2*sum(log(abs(diag(object$chol))))
}

vars.unscaled.pred.pen.fit.choleski <- function(object, newdata) {
  diag(newdata%*%object$cov.unscaled%*%t(newdata))
}

##########################################################################
# QR-DECOMPOSITION-BASED DRIVER CLASS

# Function which computes the inverse of R'R
qr2inv <- function(qr) {
  r <- ncol(qr$qr)
  R <- qr$qr[1:r,]
  R[rep(1:r, r)>rep(1:r,each=r)] <- 0
  # There is a more efficient way ... (to be coded in C if required)
  R.inv <- solve(R)
  R.inv%*%t(R.inv)
}

initialise.fit.pen.fit.qr <- function(object, ...) {
  # This method can't compute much ahead of time,
  # as it requires lambda for everything.
  if (is.null(object$D))
      stop("D (instead of DtD) needs to be provided to QR-based driver")
    object$fake.y <- c(object$y, rep(0, nrow(object$D)))
  object$BtB <- crossprod(object$B)
  object
}

update.lambda.pen.fit.qr <- function(object, lambda, ...) {
  object$qr <- qr(rbind(object$B, sqrt(lambda) * object$D))
  if (object$qr$rank<ncol(object$B))
    stop("Penalised model (numerically) rank deficient.")
  object$cov.unscaled <- qr2inv(object$qr)
  object
}

coef.pen.fit.qr <- function(object, ...) {
  coef <- qr.coef(object$qr,object$fake.y)
  names(coef) <- NULL
  coef
}

effective.df.pen.fit.qr <- function(object, ...) {
  effective.df.pen.fit.choleski(object)
}

residual.df.pen.fit.qr <- function(object, ...) {
  residual.df.pen.fit.choleski(object)
}

log.post.det.pen.fit.qr <- function(object, ...) {
  -2*sum(log(abs(diag(object$qr$qr))))
}

vars.unscaled.pred.pen.fit.qr <- function(object, newdata) {
  vars.unscaled.pred.pen.fit.choleski(object, newdata)
}


##########################################################################
# GENERALISED SVD-BASED DRIVER CLASS

# Decomposes B=Udiag(sqrt(d))X' and DtD=Xdiag(e)X'
gsvd2 <- function(B, DtD, tol=sqrt(.Machine$double.eps)) {
  BtB <- crossprod(B)
  P.eigen <- eigen(BtB+DtD)
  if (any(P.eigen$values<tol*max(P.eigen$values)))
    stop("Singularity detected. No well-defined estimate.")
  Mt <- t(P.eigen$vectors)*(1/sqrt(P.eigen$values))
  Q.svd <- svd(B%*%t(Mt),nu=ncol(B), nv=ncol(B))
  d <- c(pmin(Q.svd$d,1), rep(0, ncol(B)-length(Q.svd$d)))
  list(U=Q.svd$u, Xtinv=t(t(P.eigen$vectors)*sqrt(1/P.eigen$values))%*%Q.svd$v,
      d=d^2, e=1-d^2, rank.B=length(Q.svd$d),
      log.det.XtX=sum(log(P.eigen$values)))
}
         
# Decomposes B=Udiag(sqrt(d))X' and DtD=Xdiag(e)X' (without ever computing DtD)
gsvd <- function(B, D) {
  P.svd <- svd(rbind(B,D))
  if (length(P.svd$d)<ncol(B))
    stop("Singularity detected. No well-defined estimate.")
  Mt <- t(P.svd$v)*(1/P.svd$d)
  Q.svd <- svd(B%*%t(Mt),nu=ncol(B), nv=ncol(B))
  d <- c(pmin(Q.svd$d,1), rep(0, ncol(B)-length(Q.svd$d)))
  list(U=Q.svd$u, Xtinv=t(t(P.svd$v)*(1/P.svd$d))%*%Q.svd$v,
      d=d^2, e=1-d^2, rank.B=length(Q.svd$d),
      log.det.XtX=2*sum(log(abs(P.svd$d))))
}

initialise.fit.pen.fit.gsvd <- function(object, ...) {
  # Compute the generalised SVD
  if (is.null(object$D)) {
    object$gsvd <- gsvd2(object$B, object$DtD)
  } else {
    object$gsvd <- gsvd(object$B, object$D)
  }
  object$rank.D <- sum(object$gsvd$e>sqrt(.Machine$double.eps))
  object$z <- drop(t(object$gsvd$U)%*%object$y)
  object
}

coef.pen.fit.gsvd <- function(object, ...) {
  sel <- 1:object$gsvd$rank.B
  drop(object$gsvd$Xtinv[,sel]%*%(object$z*sqrt(object$gsvd$d[sel]) / (object$gsvd$d[sel]+object$lambda*object$gsvd$e[sel])))
}

update.lambda.pen.fit.gsvd <- function(object, lambda, ...) {
  # There is nothing we need to do as lambda changes
  object
}

effective.df.pen.fit.gsvd <- function(object, ...) {
  sum(object$gsvd$d/(object$gsvd$d + object$lambda*object$gsvd$e))
}

residual.df.pen.fit.gsvd <- function(object, ...) {
  nrow(object$B) - 2*effective.df(object) + sum((object$gsvd$d/(object$gsvd$d + object$lambda*object$gsvd$e))^2)
}

log.post.det.pen.fit.gsvd <- function(object, ...) {  
  -sum(log(object$gsvd$d+object$lambda*object$gsvd$e))-object$gsvd$log.det.XtX
}

prepare.vars.unscaled.pred.pen.fit.gsvd <- function(object, newdata) {
  (newdata%*%object$gsvd$Xtinv)^2
}

calculate.vars.unscaled.pred.pen.fit.gsvd <- function(object, prepared) {
  drop(prepared %*% (1 / (object$gsvd$d + object$lambda * object$gsvd$e)))
}

vars.unscaled.pred.pen.fit.gsvd <- function(object, newdata) {
  if (!is.null(object$prepared)) {
    prepared <- object$prepared
  } else {
    prepared <- prepare.vars.unscaled.pred.pen.fit.gsvd(object, newdata)
  }
  calculate.vars.unscaled.pred.pen.fit.gsvd(object, prepared)
}

##########################################################################
# GENERALISED EIGENVALUE-BASED DRIVER CLASS

# Decomposes BtB=Xdiag(d)X' and DtD=Xdiag(e)X'
gev <- function(BtB, DtD, tol=sqrt(.Machine$double.eps)) {
  P.eigen <- eigen(BtB+DtD)
  if (any(P.eigen$values<tol*max(P.eigen$values)))
    stop("Singularity detected. No well-defined estimate.")
  Mt <- t(P.eigen$vectors)*(1/sqrt(P.eigen$values))
  Q.eigen <- eigen(Mt%*%BtB%*%t(Mt))
  d <- pmin(pmax(Q.eigen$values,0),1)
  list(Xtinv=t(t(P.eigen$vectors)*sqrt(1/P.eigen$values))%*%Q.eigen$vectors,
       d=d, e=1-d, rank.B=sum(d>tol),
       log.det.XtX=sum(log(P.eigen$values)))
}

initialise.fit.pen.fit.gev <- function(object, ...) {
  # Compute the generalised eigen-decompoition to diagonalise both
  # BtB and DtD
  if (is.null(object$DtD)) {
    DtD <- t(object$D)%*%object$D
  } else {
    DtD <- object$DtD
  }
  BtB <- t(object$B)%*%object$B
  object$gsvd <- gev(BtB, DtD)
  object$rank.D <- sum(object$gsvd$e>sqrt(.Machine$double.eps))
  object$z <- t(object$gsvd$Xtinv)%*%(t(object$B)%*%object$y)
  object
}

coef.pen.fit.gev <- function(object, ...) {
  drop(object$gsvd$Xtinv%*%(object$z/(object$gsvd$d+object$lambda*object$gsvd$e)))
}

update.lambda.pen.fit.gev <- function(object, lambda, ...) {
  # There is nothing we need to do as lambda changes
  object
}

effective.df.pen.fit.gev <- function(object, ...) {
  effective.df.pen.fit.gsvd(object, ...)
}

residual.df.pen.fit.gev <- function(object, ...) {
  residual.df.pen.fit.gsvd(object, ...)
}

log.post.det.pen.fit.gev <- function(object, ...) {  
  log.post.det.pen.fit.gsvd(object, ...)
}

vars.unscaled.pred.pen.fit.gev <- function(object, newdata) {
  vars.unscaled.pred.pen.fit.gsvd(object, newdata)
}

##########################################################################
# DRIVER CLASS FOR KERNEL METHODS (i.e. B is the identity)

initialise.fit.pen.fit.kernel <- function(object, ...) {
  if (is.null(object$DtD))
    object$DtD <- crossprod(object$D)
  object$eigen <- eigen(object$DtD)
  object$z <- t(object$eigen$vectors)%*%object$y
  object$rank.D <- nrow(object$DtD)
  object  
}

coef.pen.fit.kernel <- function(object, ...) {
  values <- pmax(object$eigen$values, 0)
  drop(object$eigen$vectors %*% ( (1/(values+object$lambda)) * object$z))
}

fitted.pen.fit.kernel <- function(object, ...) {
  values <- pmax(object$eigen$values, 0)
  drop(object$eigen$vectors %*% ( (values/(values+object$lambda)) * object$z))
}

update.lambda.pen.fit.kernel <- function(object, lambda, ...) {
  # There is nothing we need to do as lambda changes
  object
}

effective.df.pen.fit.kernel <- function(object, ...) {
  values <- pmax(object$eigen$values, 0)
  sum(values / (values + object$lambda))
}

residual.df.pen.fit.kernel <- function(object, ...) {
  values <- pmax(object$eigen$values, 0)
  sum(values / (values + object$lambda))
  nrow(object$DtD) - 2*  sum(values / (values + object$lambda)) + sum(values^2 / (values + object$lambda)^2)
}

log.post.det.pen.fit.kernel <- function(object, ...) {
  values <- pmax(object$eigen$values, .Machine$double.eps)
  sum(log(values)-log(values+object$lambda))
}

vars.unscaled.pred.pen.fit.kernel <- function(object, newdata) {
  stop("Not yet implemented")
}

##########################################################################
# MODEL-AVERAGED CLASS

lambdasapply <- function(object, lambdas, caches, f, ...) {
  result <- numeric(length(lambdas))
  for (i in 1:length(lambdas)) {
    object <- update.lambda(object, lambdas[i], cache=caches[[i]])
    result[i] <- f(object, ...)
  }
  result
}

lambdamapply <- function(object, lambdas, caches, f, ...) {
  for (i in 1:length(lambdas)) {
    object <- update.lambda(object, lambdas[i], cache=caches[[i]])
    tmp <- f(object, ...)
    if (i==1)
      result <- matrix(ncol=length(tmp), nrow=length(lambdas))
    result[i,] <- tmp
  }
  result
}

lambda1stsapply <- function(object, lambdas, f, ...) {
  result <- numeric(length(lambdas))
  caches <- list()
  for (i in 1:length(lambdas)) {
    object <- update.lambda(object, lambdas[i])
    object <- cache.result(object, c("coef", "fitted", "estimate.sd"))
    caches[[i]] <- object$cache
    result[i] <- f(object, ...)
  }
  list(result=result, caches=caches)
}


pen.fit.avg <- function(object, n.points=20,  ...) {
  # Create a basic pen.fit object
  if (missing(object) || is.null(object))
    object <- pen.fit(...)
  object <- best.lambda(object, log.post, ...)
  # Find the MAP
  lambda.map <- object$lambda
  sd <- sqrt(1/abs(object$crit.hessian.logscale))
  # Compute support points for numerical integration
  require(glmmML)
  quadrature <- ghq(n.points)
  result <- list()
  result$log.lambdas <- log(lambda.map) + sqrt(2) * sd * quadrature$zeros
  result$lambda.map <- lambda.map
  # Cache results here ... (if used)
  tmp <- lambda1stsapply(object, exp(result$log.lambdas), log.post, ...)
  result$caches <- tmp$caches
  result$log.posts <- tmp$result
#  result$log.posts <- sapply(exp(result$log.lambdas), function(lambda) log.post(object, lambda) )
  posts <- exp(result$log.posts - max(result$log.posts))
  result$weights <- quadrature$weights * posts * exp(result$log.lambdas)
  result$weights <- result$weights / sum(result$weights)
  result$object <- object
  result$cache <- list()
  class(result) <- "pen.fit.avg"
  result
}

all.coef <- function(object, ...) {
  UseMethod("all.coef")
}

all.coef.pen.fit.avg <- function(object) {
  if (!is.null(object$cache$all.coef))
    return(object$cache$all.coef)
  lambdamapply(object$object, exp(object$log.lambdas), object$caches, coef)
#  matrix(unlist(lapply(exp(object$log.lambdas), function(lambda) coef(object$object, lambda))),ncol=length(object$log.lambdas))
}

coef.pen.fit.avg <- function(object) {
  if (!is.null(object$cache$coef))
    return(object$cache$coef)
  apply(all.coef(object)*object$weights,2,sum)
}

fitted.pen.fit.avg <- function(object) {
  if (!is.null(object$cache$fitted))
    return(object$cache$fitted)
  object$object$B%*%coef(object)
}

residuals.pen.fit.avg <- function(object) {
  if (!is.null(object$cache$residuals))
    return(object$cache$residuals)
  object$object$y - fitted(object)
}

predict.pen.fit.avg <- function(object, newdata, type="response", lambda, level=0.95) {
  # Check for newdata
  if (missing(newdata)) {
    if (is.null(object$object$B)) {
      stop("Training data not retained. You need to specify newdata.")
    } else {
      newdata <- object$object$B
    }
  }    
  # Match the type
  types <- c("response","se","se.prediction","confidence","prediction")
  type <- pmatch(type,types)
  if (is.na(type))
    stop("Invalid type argument.")
  type <- types[type]  
  if (type=="response") {
    return(drop(newdata%*%coef(object)))
  }
  if (type %in% c("se","se.prediction")) {
    # Compute the variance
    overall.mean <- predict(object, newdata, type="response")
    individual.means <- newdata%*%t(all.coef(object))
    if (inherits(object$object,"pen.fit.gsvd")|inherits(object$object,"penfit.gev")) {
      # There is a faster way for gev-based and gsvd-based drivers
      prepared <- prepare.vars.unscaled.pred.pen.fit.gsvd(object$object, newdata)
      individual.sds <- lambdamapply(object$object, exp(object$log.lambdas), object$caches, function(object) estimate.sd(object) * sqrt(calculate.vars.unscaled.pred.pen.fit.gsvd(object, prepared)))
   } else {
      individual.sds <- lambdamapply(object$object, exp(object$log.lambdas), object$caches, predict, newdata, type="se")
    }
    between.v <- apply(t(individual.means-overall.mean)^2*object$weights,2,sum)
    within.v <- apply(individual.sds^2*object$weights,2,sum)
    v <- between.v + within.v
    if (type=="se.prediction")
      v <- v+1
    return(sqrt(v))
  }
  # Handle confidence and prediction intervals (ADD AN EXACT METHOD AS WELL, WHICH NUMERICALLY COMPUTED THE QUANTILES)
  if (type %in%  c("confidence","prediction")) {
      response <- predict(object, newdata, type="response")
    if (type=="prediction") {
      se <- predict(object, newdata, type="se.prediction")
    } else { 
      se <- predict(object, newdata, type="se")
    }
    return(response + qt(1-(1-level)/2, df=max(1,floor(residual.df(object))))* cbind(lower=-se,upper=se))
  }   
}


effective.df.pen.fit.avg <- function(object) {
  if (!is.null(object$cache$effective.df))
    return(object$cache$effective.df)
  sum(lambdasapply(object$object, exp(object$log.lambdas), object$caches, effective.df)*object$weights)
}

residual.df.pen.fit.avg <- function(object) {
  if (!is.null(object$cache$residual.df))
    return(object$cache$residual.df)
  sum(lambdasapply(object$object, exp(object$log.lambdas), object$caches, residual.df)*object$weights)
}

log.post.pen.fit.avg <- function(object) {
  if (!is.null(object$cache$log.post))
    return(object$cache$log.post)
  log(sum(exp(lambdasapply(object$object, exp(object$log.lambdas), object$caches, log.post))*object$weights))
}

cache.result.pen.fit.avg <- function(object, what,...) {
  lst <-as.list(match.call(expand.dots = TRUE))
  lst[[1]] <- NULL
  lst$what <- NULL
  for (name in what) {
    func <- get(name, mode="function")
    result <- eval(as.call(c(func, lst)), envir=parent.frame())
    result.name <- attr(result,"slot")
    if (!is.null(result.name))
      name <- result.name
    object$cache[[name]] <- result
  }
  object
}

