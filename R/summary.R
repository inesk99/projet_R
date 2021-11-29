#'Function summary.Reg.Logistique
#'
#'summary.Reg.Logistique allows us to print logistic function
#'
#' @param object S3 type Reg_Logistique
#' @param ... unknowns parameters
#'
#' @importFrom stats quantile
#' @export
summary.Reg_Logistique = function(object, ...){

  #Quantile
  q = stats::quantile(object$deviance)
  names(q)=c("Min","Q1","Median","Q3","Max")

  #Affichage
  cat("Call :", object$call,"\n","\n")
  cat("Deviance residuals : \n")
  print(q)
  cat("\n")
  cat("Coefficients : \n")
  print(object$coef)
  cat("Degrees of Freedom :", object$ddl, "\n")
  cat("Deviance :", -2*sum(object$deviance), "\n" )
}


