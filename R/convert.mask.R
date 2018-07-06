`convert.mask` <-
  function(x, mask){

    if (is(x, "spectrum")){
      x <- x$lambda
    }
    
    if (is.logical(mask) & length(mask) == length(x)){
      return(invisible(mask))
    } else if (is.logical(mask) & length(mask) == 1){
      return(invisible(rep(mask, length(x))))
    } else {
      if (is.character(mask)) 
        mask <- read.table(mask, header=TRUE)
      
      if (is.data.frame(mask)) {
        mask.string <- paste(apply(mask, 1,
                                   function(x){
                                     paste("(x >=", x[1], "& x <=", x[2], ")")}),
                             collapse=" | ")
        mask <- eval(parse(text=mask.string))
        return(invisible(!mask))
      } else {
        cat("Don't know what to do with mask -- assuming TRUE\n")
        return(invisible(rep(TRUE, length(x))))
      }
    }
  }

