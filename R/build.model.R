### Build a model from a template spectrum x
### Determine the multiplicative and additive corrections according to
### Kelson (2000) 
build.model <- function(x, y, p, LOSVD=c("gauss", "gh", "gauss-hermite",
                                   "shift"),
                        o.mult=4, o.add=6, p.mult=NULL, p.add=NULL){

  LOSVD <- match.arg(LOSVD)

  if (is.null(p.mult)){
    p.mult <- cbind(rep(1, length(y$lambda)), poly(y$lambda, o.mult))
  } else {
    o.mult <- dim(p.mult)[2]
  }
  if (is.null(p.add)){
    p.add <- cbind(rep(1, length(y$lambda)), poly(y$lambda, o.add))
  } else {
    o.add <- dim(p.add)[2]
  }

  if (LOSVD == "shift"){
    x.conv <- x;
    x.conv$lambda <- x.conv$lambda/(1+p[1])
  } else {
    x.conv <- convolve.spectrum.velocity(x, p, LOSVD=LOSVD)
  }
  x.conv <- resample.spectrum(x.conv, y)
  
  temp.data <- data.frame(y=y$value,
                          x=x.conv$value,
                          p.add=p.add,
                          p.mult=p.mult
                          )
  
  lm.fit <- lm(y ~  I(p.add) + I(x*p.mult) - 1,
               data = temp.data
               )
  y$template <- x$object
  y$sptype <- x$sptype
  y$Teff <- x$Teff
  y$FeH <- x$FeH
  y$logg <- x$logg
  y$RV <- x$RV
  y$template.file <- x$name
  y$v <- p[1]
  y$sigma <- p[2]
  y$h3 <- ifelse (LOSVD == "gauss", 0, p[3])
  y$h4 <- ifelse (LOSVD == "gauss", 0, p[4])
  y$o.mult <- o.mult
  y$o.add <- o.add
  
  y$model <- predict(lm.fit,
                     newdata = temp.data
                     )
  y$residuals <- y$value - y$model
  y$lm.fit <- lm.fit
  class(y) <- c("kinfit", class(y))
  
  invisible(y)
}
