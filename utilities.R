# Wrapper function for rp.spacetime.
# This is not the same as the plot method in sm-st, it does not
# fade out what is outside the convex full (though the code is very similar)

# It also needs a patched version of rp.plot3d (in rp.plot3d.R), which needs
# to be sourced AFTER loading rpanel. The patched version does not produce
# the "blocked" boundary, which the official rpanel version does. 
# The main difference is that the user can supply a masking function, rather
# than a masking matrix, so that scaling up does not produce blocks.

rp.st.wrapper <- function(model, lambda, brks, title="", time0, palfun=terrain.colors, res=30, ...) {
  X <- model$X[,1:2]
  Time <- model$X[,3]
  y <- model$y
  xgrid    <- seq(min(X[,1]), max(X[,1]), length = res)
  ygrid    <- seq(min(X[,2]), max(X[,2]), length = res)
  zgrid    <- seq(min(Time),  max(Time),  length = res)
  Xnew     <- as.matrix(expand.grid(xgrid, ygrid, zgrid))
  if (inherits(model,"smst")) {
    if (!missing(lambda)) {
      model <- update.lambda(model, lambda, ...)
      cat("Note: lambda set to",model$pen.fit.model$lambda,"\n")
    }
    pred     <- predict(model, Xnew[ , 1:2], Xnew[ , 3]) 
    pred <- pmin(max(brks), pred)
    pred <- pmax(min(brks), pred)
    pred     <- array(pred,     dim = rep(res, 3))
    
    inside.chull2D <- function(x, x0) {
      ch     <- chull(x)
      hull   <- x[ch, , drop=FALSE]
      x0     <- x0[ , 1] + x0[ , 2] * 1i
      hull   <- hull[ , 1] + hull[ , 2] * 1i  
      D      <- outer(hull, x0, "-")
      E      <- Arg(D[-1,     , drop = FALSE] / 
                    D[-nrow(D),,drop = FALSE])
      result <- colSums(E) + Arg(D[1, ] / D[nrow(D), ])
      abs(result+2*pi) < sqrt(.Machine$double.eps)
    }
    mask <- array(NA, dim = rep(res, 3))
    for (i in 1:res) {
      ind  <- abs(Time - time0) < sd(Time)*4/5 
      ind1 <- abs(Xnew[ , 3] - zgrid[i]) < diff(zgrid)[1] * 0.5
      mask[ , , i] <- inside.chull2D(X[ind, 1:2], Xnew[ind1, 1:2])
    }
    mask      <- as.numeric(mask)
    ind       <- (mask == 0)
    mask[ind] <- NA
    mask      <- array(as.numeric(mask), dim = rep(res, 3))
    
    print(range(pred))
    Model     <- list(x = cbind(xgrid, ygrid), z = zgrid, 
                      y = array(pred,     dim = rep(res, 3)))
    Model$mask.fun <- function(y, x1, x2, z0, zsd) {
      inside.chull2D <- function(x, x0) {
        ch     <- chull(x)
        hull   <- x[ch, , drop=FALSE]
        x0     <- x0[ , 1] + x0[ , 2] * 1i
        hull   <- hull[ , 1] + hull[ , 2] * 1i  
        D      <- outer(hull, x0, "-")
        E      <- Arg(D[-1,     , drop = FALSE] / 
                      D[-nrow(D),,drop = FALSE])
        result <- colSums(E) + Arg(D[1, ] / D[nrow(D), ])
        abs(result+2*pi) < sqrt(.Machine$double.eps)
      }
      mask <- array(NA, dim = c(length(x1), length(x2)))
      Xnew <- expand.grid(x1,x2)
      ind  <- inside.chull2D(X[abs(Time - z0) < 4* zsd,], Xnew)
      y[!ind] <- NA
      y
    }
  } else {
    Model <- NULL
  }
  brks <- (rep(brks, each = 2)[-1] + rep(brks, each = 2)[-length(brks)*2]) / 2
  brks <- (rep(brks, each = 2)[-1] + rep(brks, each = 2)[-length(brks)*2]) / 2
  lbls <- c(brks, NA)
  brks <- c(brks, Inf)
  col.palette <- palfun(length(brks) - 1)

  par(mar=rep(0.1,4))
  rp.spacetime(X, Time, y, Model,
     col.palette = col.palette,
     col.breaks = brks, col.labels = brks,
     zlab = "Time", ylab = title, panel=FALSE, time.window.pars = c(time0, sd(Time)/10), x1lab="", x2lab="", x1axt="n", x2axt="n", time.window="uniform")
}

# Wrapper for rp.colour.key

rp.ck.wrapper <- function(brks, palfun=terrain.colors, par.mar=c(3, 1, 1, 1.5) + 0.1,natural=TRUE,...) {
  brks <- (rep(brks, each = 2)[-1] + rep(brks, each = 2)[-length(brks)*2]) / 2
  brks <- (rep(brks, each = 2)[-1] + rep(brks, each = 2)[-length(brks)*2]) / 2
  lbls <- c(brks, NA)
  brks <- c(brks, Inf)
  col.palette <- palfun(length(brks) - 1)
  rp.colour.key(col.palette, brks, par.mar, natural = natural,...)
}

# Slightly modified version of rp.colour.key from rpanel
# Allows suppressing the axis. 

rp.colour.key <- 
function (cols, brks, par.mar = c(5, 0, 4, 3) + 0.1, natural = TRUE, 
    margin = FALSE, plot.axis=TRUE,cex=1) 
{
    ngrid <- length(cols)
    xvec <- rep(0, ngrid)
    if (length(brks) == 2) 
        brks <- seq(brks[1], brks[2], length = ngrid + 1)
    else if (length(brks) != ngrid + 1) 
        stop("inappropriate length of brks in rp.colour.key.")
    if (!natural) {
        zlim <- c(0, ngrid)
        brks.orig <- brks
        brks <- 0:ngrid
        yaxs <- "i"
    }
    else {
        zlim <- range(brks)
        yaxs <- "r"
    }
    par(mar = par.mar, mgp = c(1.5, 0.2, 0), tcl = -0.2)
    xrange <- if (margin) 
        c(-1, 1)
    else c(0, 1)
    plot(xrange, zlim, type = "n", axes = FALSE, xaxs = "i", 
        yaxs = yaxs, xlab = " ", ylab = " ")
    if (natural) 
        axis(4)
    else {
        ticks <- pretty(0:ngrid)
        lbls <- as.character(signif(brks.orig))[match(ticks, 
            0:ngrid)]
        lbls[lbls == "Inf"] <- NA
        if (plot.axis) axis(4, at = ticks, labels = lbls,cex.axis=cex)
    }
    nbrks <- length(brks)
    brks[c(1, nbrks)] <- par()$usr[3:4]
    rect(xvec, brks[-nbrks], xvec + 1, brks[-1], col = cols, 
        border = NA)
    lines(c(0, 0, 1, 1, 0), brks[c(1, nbrks, nbrks, 1, 1)])
    invisible()
}




# Function to create a "nonlinear" colour palette

mod.sqrt <- function(x) {
  x <- x-max(x)*(1-max(brks)/diff(range(brks))+0.05)
  x <- sign(x)*abs(x)^0.75
  x <- x+min(x)
}

stretch.palette <- function(palette, stretch.fun=sqrt, new.len=length(palette)) {
  seq <- 1:new.len
  seq <- stretch.fun(seq)
  seq <- (seq-min(seq))/(max(seq)-min(seq))
  seq <- round(1+(length(palette)-1)*seq)
  palette[seq]
}


# Code for optimising lambda by cross-validation
compute.cv <- function(data, nseg, lambdas, folds=10, by.well=TRUE) {
  with(data, {
  X <- cbind(X1,X2)
  well <- X[,1]+1i*X[,2]
  well <- apply(outer(well,unique(well),"=="),1,which)
  if (by.well) {
    n.wells <- max(well)
    sizes <- floor(n.wells/folds)
    sorter <- rep(1:folds, sizes)
    if (folds*sizes<n.wells)
      sorter <- c(sorter,1:(n.wells - folds*sizes))
    sorter <- sample(sorter)
    selector <- outer(sorter[well], 1:folds, "!=")
  } else {
    n.obs <- nrow(X)
    sizes <- floor(n.obs/folds)
    sorter <- rep(1:folds, sizes)
    if (folds*sizes<n.obs)
      sorter <- c(sorter,1:(n.obs - folds*sizes))
    sorter <- sample(sorter)
    selector <- outer(sorter, 1:folds, "!=")
  }
  results <- matrix(nrow=folds, ncol=length(lambdas))
  for (i in 1:folds) {
    deselected.obs <- !selector[,i]
    selected.obs <- selector[,i]
    cat("Processing fold",i,"(Model fitting) \n")
    cv.model <- sm.st(X[selected.obs,], Time[selected.obs], y[selected.obs], lambda=1, 
                      pord = 1, nseg = nseg, bdeg = 2,
                      lambda.rel.time = 1)
    cat("Processing fold",i,"(Predicting) ")
    newB <- predict(cv.model, X[deselected.obs,], Time[deselected.obs], type="design")
    for (j in 1:length(lambdas)) {
      results[i,j] <- sum((y[deselected.obs]-predict(cv.model, newB=newB, lambda=lambdas[j]))^2)
      cat(".")
    }
    cat("\n")
  }
  cv <- apply(results,2,sum)/nrow(X)
  return(cv)
  })
}


# Modified version of st.matrices from sm-st.
# Only does 2D, but returns design matrix for derivatives as well.
s.matrices <- function(x, xrange, nseg, bdeg = 3, pord = 2, lambda.rel=rep(1,2),
                            computeP = TRUE,  computeD=FALSE, sparsePenalty = FALSE, deriv=0) {

    # Compute a set of basis functions and a penalty matrix associated with x.
    # An intercept term and the main effect of any interaction terms are removed


    if (missing(xrange))
      xrange <- t(apply(x,2,range))

    if (sparsePenalty)
      require(Matrix)

    n    <- nrow(x)
    if (missing(nseg)) nseg <- rep(7, 2)
    
    # Compute B-spline basis
    
    b <- list(length = 2)
    m <- vector(length = 2)
    for (i in 1:2) {
       b[[i]] <- bbase(x[,i], xl = xrange[i , 1], xr = xrange[i, 2], nseg = nseg[i], 
                       deg = bdeg)
       m[i]   <- ncol(b[[i]])
    }

    B <- t(apply(cbind(b[[1]], b[[2]]), 1,
                            function(x) c(x[1:m[1]] %x% x[-(1:m[1])])))
    
    result <- list(B = B, xrange = xrange, nseg = nseg, bdeg = bdeg, pord = pord)

    if (computeP || computeD) {
      # Construct smoothness penalty matrices
      D <- list(length = 2)
      for (i in 1:2) {
        D[[i]] <- sqrt(lambda.rel[i]) * diff((if (sparsePenalty) Diagonal else diag)(m[i]), diff = pord)
      }
      if (computeP)
        result$P <- crossprod(D[[1]]) %x% diag(m[2]) +
                    diag(m[1]) %x% crossprod(D[[2]])
      if (computeD)
        result$D <- (if (sparsePenalty) rBind else rbind)(
                       D[[1]] %x% diag(m[2]) ,
                       diag(m[1]) %x% D[[2]])
    }
  
    if (deriv>0) {
      bd <- list(length = 2)
      md <- vector(length = 2)
      for (i in 1:2) {
        bd[[i]] <- bbase(x[,i], xl = xrange[i , 1], xr = xrange[i, 2], nseg = nseg[i], 
                       deg = bdeg-deriv)%*%diff(diag(nseg[i]+bdeg))*nseg[i]^deriv/diff(xrange[i,])^deriv
        md[i]   <- ncol(bd[[i]])
      }

      result$Bx <- t(apply(cbind(bd[[1]], b[[2]]), 1,
                           function(x) c(x[1:m[1]] %x% x[-(1:m[1])])))
      result$By <- t(apply(cbind(b[[1]], bd[[2]]), 1,
                           function(x) c(x[1:m[1]] %x% x[-(1:m[1])])))
     
    }
      

    
    invisible(result)
}

# Function which expands the range by a fixed proportion
expand.range <- function(x,expand=1) {
  r <- range(x)
  c(r[1]-expand*(r[2]-r[1]),r[2]+expand*(r[2]-r[1]))
}

