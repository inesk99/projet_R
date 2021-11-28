#'Function print.Reg.Logistique
#'
#'print.Reg.Logistique allows us to print logistic function
#'
#' @param x S3 type Reg_Logistique
#' @param ... unknown params
#'
#'
#' @export
print.Reg_Logistique = function(x,...){

  #Affichage
  print(cat("Call :", x$call,"\n","\n"))
  print(cat("Coefficients :","\n"))
  print(x$coef)
  print(cat("\n"))
  print(cat("Degrees of Freedom :", x$ddl, "\n"))
  print(cat("Deviance :", -2*sum(x$deviance), "\n" ))

}

