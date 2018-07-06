prepare.IndoUS <- function(ident, config.file=NULL, path=NULL,
                           lambda.lim=NULL, resolution.target=NULL, ...){

  configuration <- NULL
  
  if (!missing(config.file)){
    source(config.file, local=TRUE)
    if (missing(lambda.lim))
      lambda.lim <- configuration$lambda.temp.lim
    if (missing(ident))
      ident <- configuration$template.file
    if (is.null(resolution.target))
      resolution.target <- configuration$resolution
  }

  resolution.IndoUS <- 0.43

  if (!is.null(resolution.target)){
    sigma.smooth <- sqrt(resolution.target^2 - resolution.IndoUS^2)
    cat(paste("Smoothing by", sigma.smooth, "\n"))
  } else {
    sigma.smooth <- NULL
  }
  
  if (missing(path))
    path <- paste(Sys.getenv("SLACS_HOME"), "/Indo-US/IRAF/", sep="")

### 2013-04-11: Index [0] gives error, works without
#  file <- paste(path, ident, ".fits[0]", sep="")
  file <- paste(path, ident, ".fits", sep="")
  
  template <- prepare.template(file=file, lambda.lim=lambda.lim,
                               smooth.kernel=sigma.smooth, ...)
  
  template$object <- system(paste("getkey", file, "OBJNAME"),
                            intern=TRUE)
  template$sptype <- system(paste("getkey", file, "PICKTYPE"),
                            intern=TRUE)
  template$FeH <- as.numeric(system(paste("getkey", file, "FEH"),
                                    intern=TRUE))
  template$Teff <- as.numeric(system(paste("getkey", file, "TEFF"),
                                     intern=TRUE))
  template$logg <- as.numeric(system(paste("getkey", file, "LOGG"),
                                     intern=TRUE))
  template$RV <- as.numeric(system(paste("getkey", file, "OBJRV"),
                                   intern=TRUE))
  
  if (!is.null(resolution.target)){
    template$resolution <- resolution.target
  } else {
    template$resolution <- resolution.IndoUS
  }
  
  cat(paste("\nRead Indo-US spectrum for", template$object, ":\n",
            "   Spectral type: ", template$sptype, "\n",
            "   Temperature:   ", template$Teff, "\n",
            "   Metallicity:   ", template$FeH, "\n",
            "   Gravity:       ", template$logg, "\n"))

  
  invisible(template)
}
    
