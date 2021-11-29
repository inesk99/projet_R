#'Function print.Reg.Logistique
#'
#'print.Reg.Logistique allows us to print logistic function
#'
#' @param x S3 type Reg_Logistique
#' @param ... unknowns parameters
#'
#'
#' @export
print.Reg_Logistique = function(x,...){

  #Affichage
  cat("Call :", x$call,"\n","\n")
  cat("Coefficients :","\n")
  print(x$coef)
  cat("\n")
  cat("Degrees of Freedom :", x$ddl, "\n")
  cat("Deviance :", -2*sum(x$deviance), "\n" )

}

