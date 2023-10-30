#' Compare two variables based on their type
#'
#' @param forms The formula that takes the 2 numeric variables.
#' @param data The data frame that contains the 2 variables.
#' @param txt A flag that indicates to display text. Default value is 1.
#' @param tbl A flag that indicates to display the number of table(s). Default value is 1.
#' @param fig A flag that indicates to display the number of figure(s). Default value is 1.
#' @param y.name The 'y' variable in the formula. Default value is NULL.
#' @param x.name The 'x' variable in the formula. Default value is NULL.
#' @param clr The color of the text. Default value is 'black'.
#' @param line The number of lines in the result. Default value is 1.
#'
#' @return returns the statistical relationship between the two variables with texts, plots and references.
#' @export
#'
#' @examples
associate <- function(forms, data, txt= 1, tbl= 1, fig= 1, y.name= NULL, x.name= NULL, clr= NULL, line= 1) {



  ### get the variables from formula

  #if(class(data) != "data.frame"){ #TODO: delete if below code works
  if(!inherits(data,"data.frame")) {
    data <- data.frame(data)
  }

  form_vars <- get.vars(forms)

  ### get variable in y
  depend_y <- form_vars$y.var
  y <- data[, depend_y]

  ### get variable in x
  independ_x <- form_vars$x.var
  x <- data[, independ_x]

  ## if both x and y variable are numerical, then use correlate()

  if ((class(x) %in% c("numeric", "double", "integer")) &&
      (class(y) %in% c("numeric", "double", "integer"))) {

    #print("use correlate method")
    res <- correlate(forms, data, txt= txt, tbl= tbl, fig= fig, y.name= NULL, x.name= NULL, clr= "red", line= line)
    class(res) <- "SBP.result"
    return (res)
  }
  ## if both of the variables are not numerical, use compare()
  else {

    #print("use compare method")
    res <- compare(forms, data, txt= txt, tbl= tbl, fig= fig, y.name= NULL, grp.name= NULL, clr= "rainbow")
    class(res) <- "SBP.result"
    return (res)

  }

}







