OII.model.map <- function(config.file, cube.fit, galspec, template,
                          plot=FALSE, m1=NULL, m2=NULL, normalize=1e-18, ...){
  
  if (missing(config.file)){
    stop("Need config.file\n")
  }

  ## prevent warning
  configuration <- list()
  
  source(config.file)

  lambda.OII <- 3727*(1+configuration$z.source)/(1+configuration$z.lens)

  print(lambda.OII)
  
  ## Read data cube unless already done
  if (missing(galspec)){
    galspec <- prepare.cube(config.file, lambda.lim=c(lambda.OII-50, lambda.OII+50), normalize=normalize)
  }
  print(length(galspec))
  if (missing(template)){
    template <- prepare.IndoUS(config.file=config.file,
                               lambda.lim=c(lambda.OII-200, lambda.OII+200))
  }


  ## Read the cube fit parameters
  if (missing(cube.fit))
    stop("Need results of the cube fit\n")
  
  if (is.character(cube.fit))
    cube.fit <- read.table(cube.fit, header=TRUE)
  

  ## Build the model
  ymod <- OII.model(galspec[[1]]$lambda, m1=m1, m2=m2)

  print(dim(cube.fit))
  print(dim(cube.fit)[1])
  
  ## Determine the OII flux
  OII.modelflux <- lapply(1:dim(cube.fit)[1], function(x){
    OII.fit(galspec[[x]], template, cube.fit[x,], model=ymod, plot=plot, ...)
  })

  print(length(OII.modelflux))
  
  ## Reshape OII.modelflux
  col.names <- names(OII.modelflux[[1]])
  OII.modelflux <- as.data.frame(t(matrix(unlist(OII.modelflux),
                                          nrow=length(col.names))))
  colnames(OII.modelflux) <- col.names

  return(OII.modelflux)
}

  
OII.model <- function(lambda, m1=5277, m2=5281.5, s1=1.2, s2=1.2, a.rel=1.5){
  dx <- median(diff(lambda))
  x1 <- lambda-dx/2
  x2 <- lambda+dx/2
  y1 <- pnorm(x2, mean=m1, sd=s1) - pnorm(x1, mean=m1, sd=s1)
  y2 <- pnorm(x2, mean=m2, sd=s2) - pnorm(x1, mean=m2, sd=s2)

  return ( list(lambda=lambda, value=y1 + a.rel*y2) )
}

OII.gof <- function(p=c(1), y, y.mod){
  sum((y - p[1]*y.mod)^2)
}
  


OII.fit <- function(galaxy, template, spec.fit, model, plot=FALSE, ...){
  
  if (any(is.nan(galaxy$value)))
    return(list(L=galaxy$L, M=galaxy$M, Flux=NA, SN=NA))
  
  lenssub <- galaxy
  if (!is.na(spec.fit$sigma)){
    lens.model <- build.model(template, galaxy, spec.fit$v, spec.fit$sigma, ...)
    lenssub$value <- lenssub$value - lens.model$value
  } else {
    lens.model <- lm(galaxy$value ~ poly(galaxy$lambda, 3))
    lenssub$value <- lens.model$residuals
  }
  Opt <- optimize(OII.gof, y=lenssub$value, y.mod=model$value,
                  interval=c(-10, 50))
  if (plot){
    plot(lenssub$lambda, lenssub$value, type='l')
    lines(model$lambda, Opt$minimum * model$value, col=2)
  }
  
  return(list(L=galaxy$L, M=galaxy$M, Flux=Opt$minimum, SN=spec.fit$SN))
}

