write.kinmap <- function(x, component, outfile, na.val=0,
                         overwrite=FALSE, 
                         configuration=NULL,
                         crval1=NULL, crval2=NULL,
                         crpix1=NULL, crpix2=NULL){

  ### Size of the matrix. Note that we're going to rotate the image!
  nx <- length(unique(x$M))
  ny <- length(unique(x$L))

  ### THIS DOES NOT WORK YET
  if (is.character(configuration)){  
    source(configuration, local=TRUE)
    print(configuration)
  }
  
  nfiles <- min(length(component), length(outfile))
  
  for (i in 1:nfiles){

    ## Reshape x
    x.m <- matrix(x[[component[i]]], ncol=nx)
    x.m <- as.vector(apply(x.m, 1, rev))
    
    write.fits(x.m,
               ifelse(overwrite, paste("!", outfile[i], sep=""), outfile[i]),
               nx, ny, na.val)

    ## Information on kinematic map
    system(paste("modhead", outfile[i], "IMTYPE", "\\\'Kinematic Map\\\'"))
    system(paste("modhead", outfile[i], "KINPARA", toupper(component[i])))

    ## Some more information from the object configuration
    if (!is.null(configuration)){
      system(paste("modhead", outfile[i], "TARGET", configuration$object)) 
      system(paste("modhead", outfile[i], "CUBE", configuration$data.cube))
      system(paste("modhead", outfile[i], "NOISE", configuration$noise.cube))
      system(paste("modhead", outfile[i], "TEMPLATE", configuration$template.file))
    }

    ## Write WCS if specified
    ### WCS IS NOT YET CORRECT, CHECK CRPIX1 AND CRPIX2
    if (!is.null(crval1) & !is.null(crval2) & !is.null(crpix1) &
        !is.null(crpix2)){
      system(paste("modhead", outfile[i], "CRVAL1", crval1))
      system(paste("modhead", outfile[i], "CRVAL2", crval2))
      ### Seems to work with 2, but why 2????
      system(paste("modhead", outfile[i], "CRPIX1", max(x$M) - crpix2 + 2))
      system(paste("modhead", outfile[i], "CRPIX2", crpix1 - min(x$L)))
      system(paste("modhead", outfile[i], "CD1_1", "-0.000183333333333333"))
      system(paste("modhead", outfile[i], "CD1_2", "0."))
      system(paste("modhead", outfile[i], "CD2_1", "0."))
      system(paste("modhead", outfile[i], "CD2_2", "0.000183333333333333"))
      system(paste("modhead", outfile[i], "CDELT1", "-0.00018333333333333"))
      system(paste("modhead", outfile[i], "CDELT2", "0.00018333333333333"))
      system(paste("modhead", outfile[i], "CTYPE1", "RA---TAN"))
      system(paste("modhead", outfile[i], "CTYPE2", "DEC--TAN"))
      system(paste("modhead", outfile[i], "LTV1", 1+max(x$M)))
      system(paste("modhead", outfile[i], "LTV2", 1 - min(x$L)))
      system(paste("modhead", outfile[i], "LTM1_1", "-1."))
      system(paste("modhead", outfile[i], "LTM2_2", "1."))
    }
  }
  invisible(0)
}
