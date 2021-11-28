#'Function centr_reduc_vecteur
#'
#'centr_reduc_vecteur function allows us to standarize our variables.
#'
#' @param x it's a param
#'
#' @return Returns standarized variables
#' @export
centr_reduc_vecteur = function(x){
  #Si x est numerique
  if (is.numeric(x)==TRUE){
    #Si l'ecart-type est different de 0
    if (sd(x)!=0){
      x = (x-mean(x))/sd(x)
    } else { #Sinon on garde 0
      return(0)
    }
  } else { #Si non num?rique, pas besoin de centrer-reduire
    return(x)
  }
}

#'Function centr_reduc_dataframe
#'
#'centr_reduc_dataframe function allows us to standarize our variables in the dataframe.
#'
#' @param X it's a dataframe
#'
#' @return Returns the new dataframe with standarized variables
#'
#' @export
centr_reduc_dataframe = function(X){
  #Application sur chaque terme de X la fonction de centrage-reduction
  transformation = lapply(X,centr_reduc_vecteur)
  #Transformation en dataframe du X
  X = data.frame(transformation)
  #On retourne X centre-reduit
  return(X)
}
