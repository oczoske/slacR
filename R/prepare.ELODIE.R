prepare.ELODIE <- function(ident, config.file=NULL, path=NULL, type="H",
                           lambda.lim=NULL, resolution.target=NULL,
                           contdiv=FALSE, contsub=FALSE, ...){
  
  configuration <- NULL
  
  if (!missing(config.file)){
    cat("Reading config file\n")
    source(config.file, local=TRUE)
    if (missing(lambda.lim))
      lambda.lim <- configuration$lambda.temp.lim
    if (missing(ident))
      ident <- configuration$template.file
    if (is.null(resolution.target))
      resolution.target <- configuration$resolution
  }

  if (type == "H"){
    resolution.ELODIE <- 0.0556
  } else if (type == "L"){
    resolution.ELODIE <- 0.234
  } else {
    cat("Unknown type - must be \"H\" or \"L\"\n")
    invisible(NULL)
  }

  if (!is.null(resolution.target)){
    sigma.smooth <- sqrt(resolution.target^2 - resolution.ELODIE^2)
    cat(paste("Smoothing by", sigma.smooth, "\n"))
  } else {
    sigma.smooth <- NULL
  }
  
  if (missing(path))
    path <- paste(Sys.getenv("SLACS_HOME"), "/ELODIE/", type, "/", sep="")

  file <- paste(path, ident, ".fits[0]", sep="")
  
  template <- prepare.template(file=file, lambda.lim=lambda.lim,
                               contdiv=contdiv, contsub=contsub,
                               smooth.kernel=sigma.smooth,...)
  template$contdiv <- template$value

  class(template) <- "spectrum"

  template$object <- system(paste("getkey", file, "OBJECT"),
                            intern=TRUE)
  template$sptype <- system(paste("getkey", file, "SPTYPE"),
                            intern=TRUE)
  template$FeH <- as.numeric(system(paste("getkey", file, "I_FE_H"),
                                    intern=TRUE))
  template$Teff <- as.numeric(system(paste("getkey", file, "I_TEFF"),
                                     intern=TRUE))
  template$logg <- as.numeric(system(paste("getkey", file, "I_LOGG"),
                                     intern=TRUE))

  if (!is.null(resolution.target)){
    template$resolution <- resolution.target
  } else {
    template$resolution <- resolution.ELODIE
  }

  
  cat(paste("\nRead ELODIE spectrum for", template$object, ":\n",
            "   Spectral type: ", template$sptype, "\n",
            "   Temperature:   ", template$Teff, "\n",
            "   Metallicity:   ", template$FeH, "\n",
            "   Gravity:       ", template$logg, "\n"))
  
  invisible(template)
}
    

