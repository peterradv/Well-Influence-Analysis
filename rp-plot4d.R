source("rp-colour-key.R")

# Returns a vector of TRUE / FALSE depending on whether x0 is in the convex hull of x
inside.chull2D <- function(x, x0) {
  ch <- chull(x)
#  browser()
  hull <- x[ch,,drop=FALSE]
  # Move to the complex plane
  x0 <- x0[,1]+x0[,2]*1i
  hull <- hull[,1]+hull[,2]*1i  
  D <- outer(hull, x0, "-")
  E <- Arg(D[-1,,drop=FALSE]/D[-nrow(D),,drop=FALSE])
  result <- colSums(E)+Arg(D[1,]/D[nrow(D),])
  abs(result+2*pi)<sqrt(.Machine$double.eps)
}


#     Plots of two covariates coloured by a response variable and 
#     animated by a third covariate.

# panel can be
# "full" / TRUE - default (equivalent to TRUE)
# "notkr" - no tkr plots (limited functionality)
# "norpanel" / FALSE - no rpanel controls shown
# "notime" / - no rpanel controls and no time bar shown
# "mainplotonly" - only main plot (with no rplanel controls shown)

rp.plot4d <- function(x, y, z, model = NA, col.palette,
                  hscale = 1, vscale = hscale, superimpose = NULL, panel = "full", key.cex=1,
                  z0 = min(z), zsd = sd(z)/5, z.window = "normal", zrange,
                  hide.data=FALSE, mask.strategy="window", mask.alpha=0.5, data.cex=1, eqscplot=FALSE, nonlinear.palette=TRUE, ... #, transformation=I 
) {

  if (eqscplot)
    require(MASS)
  require(colorRamps)
 
#  y <- transformation(y)
 
 
   draw.plot <- function(panel) {
      with(panel, {


        zsd1  <- if (zsd >= 1.49 * sdz) 4 * sdz else zsd

        
      	 if (panel.flag) {     
		            par(mar = c(3, 3, 1, 1) + 0.1, mgp = c(1.5, 0.2, 0), tcl = -0.2)
         }
         if (redraw.all.flag) {
           if (!is.null(y) && time.flag) {
             layout(rbind(c(1,0),c(2,3)),widths=c(10,1), heights=c(1,10 ))
             draw.band(panel)
           } else {
             layout(rbind(c(1,2)),widths=c(10,1))
           }
         }
         (if (eqscplot) get("eqscplot",mode="function") else plot)(x[,1],x[,2], type = "n", pty = "s")
         
         if (is.list(model)) {
 #           model$y <- transformation(model$y)
            mt     <- dim(model$y)[3]
            if (length(model$z)==1) {
              m.low <- 1
              m.high <- 1
              m.ind <- 1
              m.p <- 0              
            } else {
              m.ind  <- (z0 - min(model$z)) / (diff(range(model$z)) / (mt - 1))
              m.low  <- min(1 + floor(m.ind), mt)
              m.high <- min(1 + m.low, mt)
            }
            m.p    <- 1 + m.ind - m.low
            if (m.low >= 1 & m.high <= mt) {
               my <- (1 - m.p) * model$y[ , , m.low] + m.p * model$y[ , , m.high]


               if (require(akima)) {
                  m.in <- as.matrix(expand.grid(model$x[ , 1], model$x[ , 2]))
                  mx   <- cbind(seq(min(model$x[ , 1]), max(model$x[ , 1]), length = 150),
                                seq(min(model$x[ , 2]), max(model$x[ , 2]), length = 150))
                  grd  <- interp(m.in[ , 1], m.in[ , 2], c(my), mx[ , 1], mx[ , 2])
                  my   <- grd$z
               } else {
                 mx <- model$x
               }

               if (mask.strategy!="none") {
                 expanded.x <- expand.grid(mx[,1], mx[,2])
                 if (mask.strategy=="all") {
                   mask.selector <- 1:nrow(x)
                 }
                 if (mask.strategy=="window") {                   
                   mask.selector <- abs(z - z0) < 2 * zsd1
                 }
                 show <- inside.chull2D(x[mask.selector,,drop=FALSE],expanded.x)
                 my.range <- diff(range(brks))
                 my[!show] <- my[!show]+diff(range(brks))
                 col.hsv <- rgb2hsv(col2rgb(col.palette))
                 col.hsv <- hsv(col.hsv[1,], col.hsv[2,]*mask.alpha, col.hsv[3,])
                 col.palette.image <- c(col.palette,  col.hsv)
               } else {
                 col.palette.image <- col.palette
               }

               image(mx[ , 1], mx[ , 2], my, zlim = c(min(brks), max(brks)+diff(range(brks))), col = col.palette.image, add = TRUE)
#               print(levelplot(my, 
#                     row.values = model$x[, 1], column.values = model$x[ , 2],
#                     cuts = length(col.palette), colorkey = FALSE, at = brks,
#                     region = TRUE, col.regions = col.palette))
            }
         }

         if (is.function(superimpose)) superimpose()

         if (is.list(model)) z.window <- "uniform"

         if (!is.null(y)) {
            alpha <- exp(-0.5 * (z - z0)^2 / zsd1^2)
           ord   <- order(alpha)
           if (z.window == "normal")
             clr <- hsv(clr[1, ], clr[2, ] * alpha, clr[3, ])
           else if (z.window == "uniform") {
             clr <- hsv(clr[1, ], clr[2, ], clr[3, ])
             ord <- ord[abs(z[ord] - z0) < 2 * zsd1]             
           }
           
           points(x[ord, 1], x[ord, 2], pch = 21, bg = clr[ord], col=if ((z.window == "uniform") & is.list(model)) "black" else NA, cex=data.cex)

         }
         if (Select == xlab && all(!is.na(coords)))
            lines(coords[1] + circle[ , 1] * radius * diff(range(x[ , 1])), 
                  coords[2] + circle[ , 2] * radius * diff(range(x[ , 2])))

         if (redraw.all.flag) {
           draw.key(panel)
          }
         
      })
      panel
   }
   
   draw.key <- function(panel) {
      if (is.factor(panel$y)) {
      	 par.mar <- par()$mar
      	 par(mar = c(3, 0, 1, 2) + 0.1)
      	 plot(0:1, type = "n", axes = FALSE, xlab = "", ylab = "")
      	 legend("topleft", c("after", "before"), text.width = 1,
            col = c("green", "red"), text.col = c("green", "red"))
         par(mar = par.mar)
      }
      else {
      	 del <- diff(range(panel$y, if (!all(is.na(panel$model))) panel$model$y else c(), na.rm=TRUE)) * 0.04
         colour.key(panel$col.palette, brks, par.mar = c(3, 1, 1, 1) + 0.1,key.cex=key.cex)
         mtext(ylab, side = 2, line = 0.4, font = 1,cex=key.cex)
      }
      panel
   }
   
   draw.band <- function(panel) {
      with(panel, {
         mar.old <- par()$mar
         par(mar = c(0, 3, 2, 1) + 0.1)
         plot(range(z), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "",
               xaxs = "i", yaxs = "i")
         zsd1  <- if (zsd >= 1.49 * sdz) 4 * sdz else zsd
         if (is.list(model)) z.window <- "uniform"
         if (z.window == "normal") {
            nrect <- 100
            zvec  <- seq(par()$usr[1], par()$usr[2], length = nrect + 1)
            zmid  <- (zvec[-nrect + 1] + zvec[-1]) / 2
            alpha <- exp(-0.5 * (zmid - z0)^2 / zsd1^2)
            clr   <- rgb2hsv(col2rgb("green"))
            clr   <- hsv(rep(clr[1, ], nrect), clr[2, ] * alpha, rep(clr[3, ], nrect))
            rect(zvec[-(nrect + 1)], 0, zvec[-1], 1, col = clr, border = NA)
         }
         else if (z.window == "uniform")
            rect(z0 - 2 * zsd1, 0, z0 + 2 * zsd1, 1, col = "green", border = NA)
         axis(3, mgp = c(1, 0.5, 0), tcl = -0.1, font.main = 1)
         abline(v=z0, col="darkgreen")
         mtext(zlab, line = 1.5, font = 1)
         box()
         par(mar = mar.old)
      })
      panel
   }

   location.plot <- function(panel) {
      if (panel$Select == xlab) {
      	 panel$zsdold <- panel$zsd
      	 panel$zsd    <- panel$sdz * 4
         rp.text(panel, "\n\n", name = "textpane", grid = "plots", row = 0, column = 2,
            sticky = "news", background = "white", fontsize = 12)
         rp.tkrplot(panel, location, draw.location, grid = "plots", row = 1, column = 2)
      }
      else
         panel$zsd <- panel$zsdold
      rp.control.put(panel$panelname, panel)
      rp.tkrreplot(panel, plot)
      rp.tkrreplot(panel, band)
      panel
   }
   
   draw.location <- function(panel) {     
      with(panel, {
         if (is.null(panel$y))
           return(panel)
         par(mar = c(3, 0.5, 1, 1) + 0.1, mgp = c(1.5, 0.2, 0), tcl = -0.2)
         plot(z, y, type = "n", axes = FALSE)
         axis(1)
         axis(2, labels = FALSE)
         box()
         points(z[locind], y[locind])
      })
      if (all(!is.na(panel$coords)))
         rp.text.change(panel, "textpane",
            paste("location: ", signif(panel$coords[1], 5), ", ",
                                signif(panel$coords[2], 5), "\n"))
#                  "radius: ", signif(panel$radius, 5), sep = ""))
      else
         rp.text.change(panel, "textpane", "\n\n")
      panel
   }
   
   click <- function(panel, x, y) {
      if (panel$Select == xlab) {
         d.pts        <- ((panel$x[ , 1] - x) / diff(range(panel$x[ , 1])))^2 + 
                         ((panel$x[ , 2] - y) / diff(range(panel$x[ , 2])))^2
         panel$locind <- which(d.pts <= panel$radius^2)
         panel$coords <- c(x, y)
         rp.control.put(panel$panelname, panel)
         rp.tkrreplot(panel, plot)
         rp.tkrreplot(panel, location)
      }
      panel
   }
   
   drag <- function(panel, x, y) {
      if (panel$Select == xlab) {
         d.pts        <- ((panel$x[ , 1] - x) / diff(range(panel$x[ , 1])))^2 + 
                         ((panel$x[ , 2] - y) / diff(range(panel$x[ , 2])))^2
         panel$locind <- which(d.pts <= panel$radius^2)
         panel$coords <- c(x, y)
         rp.control.put(panel$panelname, panel)
         rp.tkrreplot(panel, plot)
         rp.tkrreplot(panel, location)
      }
      panel
   }
   
   release <- function(panel, x, y) {
      panel$locind <- integer(0)
      panel$coords <- rep(NA, 2)
      rp.control.put(panel$panelname, panel)
      rp.tkrreplot(panel, plot)
      rp.tkrreplot(panel, location)
      panel
   }

   plot.3d <- function(panel) {
      with(panel, {
         rp.plot3d(x[ , 1], x[ , 2], z, col = colour)
      })
      panel
   }

   redraw4d <- function(panel) {
      if (panel$tkr.flag) {
        rp.tkrreplot(panel, plot)
        rp.tkrreplot(panel, band)
      } else {
        draw.plot(panel)
      }
      panel
   }
   
   draw.blank <- function(panel) {
      plot(1:10, type = "n", axes = FALSE, xlab = "", ylab = "")
      panel
   }
   
   xlab <- deparse(substitute(x))
   ylab <- deparse(substitute(y))
   zlab <- deparse(substitute(z))

   if (is.logical(panel)) {
     if (panel) { panel <- "full" } else { panel <- "norpanel" }
   }

   panel.options <- c("full", "notkr", "norpanel","notime", "mainplotonly")
   panel <- pmatch(panel, panel.options)

   if (is.na(panel))
     stop("panel has to be either 'full', 'notkr', 'norpanel' or 'mainplotonly'.")
   panel <- panel.options[panel]

   if (panel=="full") {
     panel.flag <- TRUE
     redraw.all.flag <- FALSE
     tkr.flag <- TRUE     
	time.flag <- TRUE
   }
   if (panel=="notkr") {
     panel.flag <- TRUE
     redraw.all.flag <- TRUE
     tkr.flag <- FALSE
	time.flag <- TRUE
   }
   if (panel=="norpanel") {
     panel.flag <- FALSE
     redraw.all.flag <- TRUE
     tkr.flag <- FALSE
	time.flag <- TRUE
   }
   if (panel=="notime") {
     panel.flag <- FALSE
     redraw.all.flag <- TRUE
     tkr.flag <- FALSE
	time.flag <- FALSE
   }
   if (panel=="mainplotonly") {
     panel.flag <- FALSE
     redraw.all.flag <- FALSE
     tkr.flag <- FALSE
	time.flag <- FALSE
   }

   if (missing(model))
      model <- NA
   else if (!is.list(model)) {
      cat("model$y is not a list and will not be used.\n")
      model <- NA
   }      
   else if (length(dim(model$y)) != 3) {
      cat("model$y is not a three-dimensional array and will not be used.\n")
      model$y <- NA
   }

   if (is.factor(y)) {
      if (missing(col.palette) || all(is.na(col.palette)))
         col.palette <- matlab.like(nlevels(y))
      ind <- as.numeric(y)
      key <- 0.25
   }
   else {
      if (missing(col.palette) || all(is.na(col.palette))) {
         col.palette <- matlab.like(250)
       }
          if (is.list(model) & nonlinear.palette) {            
           col.palette <- nonlinear.palette(range(y),range(model$y,na.rm=TRUE),proportion=0.5, palette=col.palette)
         }
     rng  <- if (!is.list(model)) range(y) else range(y, model$y, na.rm=TRUE)
      if (!missing(zrange))
        rng <- zrange
      del  <- 0.001 * diff(rng)
      brks <- seq(rng[1] - del, rng[2] + del, length = length(col.palette) + 1)
      if (!is.null(y))
        ind  <- cut(y, brks, labels = FALSE)
      key  <- 0.15
   }
   if (!is.null(y)) {
     colour <- col.palette[ind]
   } else {
     colour <- rep("black", length(z))
   }
   clr    <- col2rgb(colour)
   clr    <- rgb2hsv(clr)
#   rad    <- mean(apply(x, 2, function(w) diff(range(w)))) / 20
   rad    <- 0.05
   theta  <- seq(0, 2 * pi, length = 50)
   circle <- matrix(c(cos(theta), sin(theta)), ncol = 2)
   n      <- length(y)

   
   
   if (panel.flag) {
      panel <- rp.control(x = x, y = y, z = z, z0 = z0,
                          xlab = xlab, ylab = ylab, zlab = zlab,
                          model = model, brks = brks,
                          col.palette = col.palette, coords = rep(NA, 2), key.cex=key.cex,
                          radius = rad, circle = circle, panel.flag = panel.flag, time.flag=time.flag, redraw.all.flag = redraw.all.flag, tkr.flag=tkr.flag,
                          Select = zlab, n = n, zsd = zsd, sdz = sd(z), 
                          colour = colour, clr = clr, z.window = z.window, eqscplot=eqscplot,
                          locind = integer(0), superimpose = superimpose)
      rp.grid(panel, "controls", row = 0, column = 0, sticky = "n")
      if (tkr.flag) {
        rp.grid(panel, "plots",    row = 0, column = 1)
        rp.tkrplot(panel, band, draw.band, click, drag, release,
                   hscale = hscale, vscale = 0.12 * vscale,
                   grid = "plots", row = 0, column = 0)
        rp.tkrplot(panel, plot, draw.plot, click, drag, release,
                   hscale = hscale, vscale = vscale,
                   grid = "plots", row = 1, column = 0, mar = c(5, 4, 1, 1) + 0.1)
        rp.tkrplot(panel, key,  draw.key, hscale = key * hscale, vscale = vscale,
                   grid = "plots", row = 1, column = 1)
        rp.tkrplot(panel, key,  draw.blank, hscale = key * hscale, vscale = 0.12 * vscale,
                   grid = "plots", row = 0, column = 1)
      }
      if (tkr.flag)
        rp.radiogroup(panel, Select, c(zlab, xlab), action = location.plot,
                      grid = "controls", row = 0, column = 0, title = "Window")
      if (!is.list(model))
        rp.radiogroup(panel, z.window, c("normal", "uniform"), action = redraw4d,
                      grid = "controls", row = 1, column = 0, title = "Window")
      rp.slider(panel, z0, min(z), max(z), redraw4d, title = "time point",
                grid = "controls", row = 2, column = 0)
      if (!is.null(y))
        rp.slider(panel, zsd, sd(z) / 20,  sd(z) * 1.5, redraw4d, title = "time band",
                  grid = "controls", row = 3, column = 0)
      if (tkr.flag) 
        rp.slider(panel, radius, rad / 5,  rad * 5, redraw4d, title = "radius",
                  grid = "controls", row = 4, column = 0)
      if (!is.null(y)) 
        rp.button(panel, plot.3d, "3D plot", grid = "controls", row = 5, column = 0)
   }
   else {
      panel <- list(x = x, y = y, z = z, z0 = z0,
                  xlab = xlab, ylab = ylab, zlab = zlab,
                  model = model, brks = brks, key.cex=key.cex,
                  col.palette = col.palette, panel.flag = panel.flag, time.flag=time.flag, redraw.all.flag = redraw.all.flag, tkr.flag=tkr.flag,
                  Select = zlab, n = n, zsd = zsd, sdz = sd(z), 
                  colour = colour, clr = clr, z.window = z.window,
                  superimpose = superimpose, eqscplot=eqscplot)
      draw.plot(panel)
   }
   
   invisible()
}

rp.spacetime <- function(space, y, time, model, col.palette = NA,
                  hscale = 1, vscale = hscale, superimpose = NULL, panel = TRUE,
                  time0 = min(time), timesd = sd(time)/5, time.window = "normal", ...) {
   rp.plot4d(space, y, time, model, col.palette,
                  hscale = 1, vscale = hscale, superimpose=superimpose, panel = panel,
                  z0 = time0, zsd = timesd, z.window = time.window, ...)
}
                  


nonlinear.palette <- function(range1, range2, proportion=0.5, palette=matlab.like(512), n.cols=length(palette)) {
  full.range <- c(min(range1[1],range2[1]), max(range1[2],range2[2]))
  core.range <- c(max(range1[1],range2[1]),  min(range1[2],range2[2]))
  full.spread <- full.range[2]-full.range[1]
  left.range <- (core.range[1]-full.range[1])/full.spread
  right.range <- (core.range[2]-full.range[1])/full.spread
  left <- min((1-proportion)/2,left.range)
  right <- max(1-(1-proportion)/2,right.range)
  y <- c(0,left,right,1)
  x <- c(0,left.range,right.range,1)
  f <- splinefun(x,y,method="monoH.FC")
  n <- length(palette)
  u <- (1:n.cols-0.5)/n.cols
  palette[round(0.5+length(palette)*f(u))]
}

  
