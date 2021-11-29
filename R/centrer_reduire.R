#'Function centr_reduc_vecteur
#'
#'centr_reduc_vecteur function allows us to standardize features variables
#'
#' @param x corresponds to each row
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
#'centr_reduc_dataframe appplies centr_reduc_vecteur on the dataframe
#'
#' @param X a dataframe cantaining the variables in the model
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

#' Fonction save.mean
#'
#' Return the mean for each column
#'
#' @param x corresponds to the column of the dataframe
save.mean = function(x){
  if(is.numeric(x)){
    x = mean(x)
  }

}
#' Fonction save.sd
#'
#' Returns the standard deviation for each column
#'
#' @param x corresponds to the column of the dataframe
save.sd = function(x){
  if(is.numeric(x)){
    x = sd(x)
  }
}
