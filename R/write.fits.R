write.fits <- function(x, outfile, nx, ny, na.val=0){

  if (missing(nx) | missing(ny)){
    nx <- dim(x)[1]
    ny <- dim(x)[2]
  } else if (length(nx) > 1){
    nx <- length(unique(nx))
    ny <- length(unique(ny))
  }

  ### Remove NA
  x[is.na(x)] <- na.val
  
  tmpfile <- paste("/tmp/slacR.tmp.", floor(10000*runif(1)), sep="")
  write.table(as.vector(x), tmpfile, col.names=FALSE, row.names=FALSE)
  command <- paste("R2fits", tmpfile, nx, ny, outfile)
  cat(paste(command, "\n"))
  if (system(paste("R2fits", tmpfile, nx, ny, outfile))){
    cat("Error from R2fits\n")
  } else {
    file.remove(tmpfile)
  }
  invisible(0)
}
