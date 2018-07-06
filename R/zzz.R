.onAttach <- function(libname, pkgname){

  print.slacR.version <- function(){
    library(help=slacR)$info[[1]] -> slacR.info
    Package <- slacR.info[pmatch("Package",slacR.info)]
    Version <- slacR.info[pmatch("Version",slacR.info)]
    Built <- slacR.info[pmatch("Built",slacR.info)]
#      um <- strsplit(version," ")[[1]]
#      version <- um[nchar(um)>0][2]
#      cat(paste("This is slacR",version,"\n"))
    packageStartupMessage(paste(Package, "\n", Version, "\n", Built, "\n"))
#    cat(paste(Package, "\n"))
#    cat(paste(Version, "\n"))
#    cat(paste(Built, "\n"))
  }
  
  
  assign("slacR.path", paste(Sys.getenv("R_LIBS"), "/slacR/bin", sep=""), envir=.GlobalEnv)
  assign("slacs.home", Sys.getenv("SLACS_HOME"), envir=.GlobalEnv)
#  assign("c.light", 299792.5, envir=.GlobalEnv)
  print.slacR.version()
}


