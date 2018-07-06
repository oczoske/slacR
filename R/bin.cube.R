bin.cube <- function(cube, bin, add=FALSE){

    ## Number of spaxels to bin
    nbin <- length(bin$L)

    ## number of spectra in input cube
    ncube <- length(cube)
    
    ## Build a data.frame with columns index, L, M in the cube
    LM <- cbind(1:ncube, t(sapply(cube, function(x){c(x$L, x$M)})))
    LM <- as.data.frame(LM)
    colnames(LM) <- c("i", "L", "M")

    ## Pick the index for the first spectrum
    ind <- LM$i[LM$L == bin$L[1] & LM$M == bin$M[1]]
    outspectrum <- cube[[ind]]

    ## Add further spectra if there are any
    if (nbin > 1){
        for (i in 2:nbin){
            ind <- LM$i[LM$L == bin$L[i] & LM$M == bin$M[i]]
            outspectrum <- spadd(outspectrum, cube[[ind]])
        }
    }

    # divide by number of spectra to obtain the mean spectrum
    if (!add){
        outspectrum <- spdiv(outspectrum, nbin)
    }
    return (outspectrum)
}

