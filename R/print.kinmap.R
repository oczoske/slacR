print.kinmap <- function(x, ...){
    ## First case: raw kinmap
    if (is.null(x$bin)){
        Lmin <- min(x$L[!is.na(x$v)])
        Lmax <- max(x$L[!is.na(x$v)])
        Mmin <- min(x$M[!is.na(x$v)])
        Mmax <- max(x$M[!is.na(x$v)])

        xtemp <- x[x$L >= Lmin & x$L <= Lmax & x$M >= Mmin & x$M <= Mmax,]

        if (is.null(xtemp$dv.low)){
            xnew <- data.frame(
                L = xtemp$L,
                M = xtemp$M,
                SN = round(xtemp$SN, 1),
                v = round(xtemp$v, 1),
                sigma = round(xtemp$sigma, 1),
                gof = round(xtemp$gof, 2))
        } else {
            xnew <- data.frame(
                L = xtemp$L,
                M = xtemp$M,
                SN = round(xtemp$SN, 1),
                v = round(xtemp$v, 1),
                dv = round((xtemp$dv.high - xtemp$dv.low)/2, 1),
                sigma = round(xtemp$sigma, 1),
                dsig = round((xtemp$dsig.high - xtemp$dsig.low)/2, 1),
                gof = round(xtemp$gof, 2))
        }
    } else {
        ## binned spectra
        if (is.null(x$dv.low)){
            xtemp <- data.frame(
                bin=x$bin,
                SN=round(x$SN, 1),
                v=round(x$v, 1),
                sigma=round(x$sigma, 1),
                gof=round(x$gof, 2))
        } else {
            xtemp <- data.frame(
                bin=x$bin,
                SN=round(x$SN, 1),
                v=round(x$v, 1),
                dv=round( (x$dv.high - x$dv.low) / 2, 1),
                sigma=round(x$sigma, 1),
                dsig=round( (x$dsig.high - x$dsig.low) / 2, 1),
                gof=round(x$gof, 2))
        }
        xnew <- unique(xtemp)
        xnew <- xnew[ order(xnew$bin), ]
    }
    print(xnew, row.names=FALSE, digits=8, ...)
}    
            
