prepare.template.list <- function(template.list, Source=c("IndoUS"),
                                  ...){

  ## template.list can be a data.table or a file containing a data table
  if (is.character(template.list)){
    template.list <- read.table(template.list, header=T)
  }

  if (Source == "IndoUS"){
    template.function <- prepare.IndoUS
  } else if (Source == "ELODIE"){
    template.function <- prepare.ELODIE
  } else if (Source == "ESI"){
    template.function <- prepare.ESI
  } else {
    template.function <- prepare.template
  }

  template.list <- lapply(template.list$template, template.function, ...)
  invisible(template.list)
  
}




#### Provisoirement: convolve and rebin spectra list
temp.conv.list <- lapply(temp.list, convolve.spectrum, 0, 230)

temp.approx <- lapply(temp.conv.list, function(x){
  temp.a <- x
  temp.a$lambda <- galaxy$lambda
  TTAP <- approxfun(x$lambda, x$value)
  temp.a$value <- TTAP(temp.a$lambda)
  temp.a})

#### How to write the lm call automatically for an arbitrary number of
#### template in template.list?
#### How to constrain the linear fit to positive weights for the templates?
#### How to build the template out of the results of the linear fit
#### and the template list?
