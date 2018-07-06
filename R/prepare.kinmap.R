`prepare.kinmap` <-
  function(file){
    kinmap <- read.table(file, header=TRUE)
    class(kinmap) <- c("kinmap", class(kinmap))
    invisible(kinmap)
  }
