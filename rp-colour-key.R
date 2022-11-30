colour.key <- function(cols, brks, par.mar = c(5, 0, 4, 3) + 0.1, natural = TRUE, key.cex=1)  {
   ngrid <- length(cols)
   xvec  <- rep(0, ngrid)
   if (length(brks) == 2)
      brks <- seq(brks[1], brks[2], length = ngrid + 1)
   else if (length(brks) != ngrid + 1)
      stop("inappropriate length of brks in colour.key.")
   if (!natural) {
      zlim <- c(0, ngrid)
      brks.orig <- brks
      brks <- 0:ngrid
   }
   else
      zlim <- range(brks)
   par(mar = par.mar)
   plot(0:1, zlim, type = "n", xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "", cex.axis=key.cex)
   if (natural)
      axis(4, mgp = c(1.5, 0.2, 0), tcl = -0.3, cex.axis=key.cex)
   else {
      ticks <- pretty(0:ngrid)
      axis(4, at = ticks, labels = as.character(signif(brks.orig))[match(ticks, 0:ngrid)],
            mgp = c(1.5, 0.2, 0), tcl = -0.1, cex.axis=key.cex)
   }
   rect(xvec, brks[-length(brks)], xvec + 1, brks[-1], col = cols, border = NA)
   box()
   invisible()  
   }
