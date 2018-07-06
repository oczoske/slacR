trim.spectrum <- function(x, lambda){
  x.trim <- x
  lambda.range <- x$lambda > lambda[1] & x$lambda < lambda[2]
  x.trim$lambda <- x$lambda[lambda.range]
  x.trim$value <- x$value[lambda.range]
  if (!is.null(x$noise) & length(x$noise) > 1){
    x.trim$noise <- x$noise[lambda.range]
  } else {
    x.trim$noise <- x$noise
  }
  if (!is.null(x$mask))
    x.trim$mask <- x$mask[lambda.range]
  
  if (!is.null(x.trim$contsub))
    x.trim$contsub <- x$contsub[lambda.range]

  if (!is.null(x.trim$contdiv))
    x.trim$contdiv <- x$contdiv[lambda.range]


  if (!is.null(x.trim$mask))
    x.trim$mask <- x$mask[lambda.range]

  return(invisible(x.trim))
}
  
