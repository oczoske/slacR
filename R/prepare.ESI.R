prepare.ESI <- function(ident, config.file=NULL, path=NULL,
                           lambda.lim=NULL, ...){

  lambda.temp.lim <- NULL
  
  if (!missing(config.file)){
    source(config.file)
    lambda.lim <- lambda.temp.lim
  }
  
  if (missing(path))
    path <- paste(Sys.getenv("SLACS_HOME"), "/ESI/", sep="")

  file <- paste(path, "sc", ident, "wc.fits", sep="")
  
  template <- prepare.template(file=file, lambda.lim=lambda.lim,
                               contsub=TRUE, ...)

  class(template) <- "spectrum"

  template$object <- ident
  template$sptype <- system(paste("getkey", file, "SPTYPE"),
                            intern=TRUE)
  template$FeH <- as.numeric(system(paste("getkey", file, "FEH"),
                                    intern=TRUE))
  template$Teff <- as.numeric(system(paste("getkey", file, "TEFF"),
                                     intern=TRUE))
  template$logg <- as.numeric(system(paste("getkey", file, "LOGG"),
                                     intern=TRUE))

  cat(paste("\nRead ESI spectrum for", template$object, ":\n",
            "   Spectral type: ", template$sptype, "\n",
            "   Temperature:   ", template$Teff, "\n",
            "   Metallicity:   ", template$FeH, "\n",
            "   Gravity:       ", template$logg, "\n"))
  
  invisible(template)
}
    
