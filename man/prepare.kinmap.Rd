\name{prepare.kinmap}
\Rdversion{1.1}
\alias{prepare.kinmap}
\title{
  Read in a kinematic map from file
}
\description{
  This function reads in a kinematic map from a file and returns it as
  class "kinmap".
}
\usage{
prepare.kinmap(file)
}
\arguments{
  \item{file}{name of a file containing a kinematic map in the form of a
  data table}
}
\value{
  an object of class \code{kinmap}, which is essentially a
  \code{data.frame}. 
}
\author{Oliver Czoske}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(file){

    kinmap <- read.table(file, header=TRUE)
    class(kinmap) <- c("kinmap", class(kinmap))
    invisible(kinmap)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{file}
