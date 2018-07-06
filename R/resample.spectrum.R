## Function to resample a spectrum "input" to a wavelength vector "target"
## This does not deal with noise.
resample.spectrum <-
  function(input, target){

    if (is(target, "spectrum")){
      lambda <- target$lambda
    } else {
      lambda <- target
    }

    result <- approx(input$lambda, input$value, lambda)
    
    output <- input
    output$lambda <- lambda
    output$value <- result$y
    if (!is.null(input$contsub)){
      result <- approx(input$lambda, input$contsub, lambda)
      output$contsub <- result$y
    }
    if (!is.null(input$contdiv)){
      result <- approx(input$lambda, input$contdiv, lambda)
      output$contdiv <- result$y
    }
    
    output$noise <- NULL

    invisible(output)
  }
