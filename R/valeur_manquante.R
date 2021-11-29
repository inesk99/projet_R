#'Function valeurs_manquantes
#'
#'valeurs_manquantes function allows you to replace or delete a certain column which contains several NA values.
#'
#' @param x corresponds to each row
#'
#' @return Returns modified rows
#'
#' @export
valeurs_manquantes = function(x){
  #vecteur avec les valeurs restantes
  y =  na.omit(x)
  #Pas de donnees manquantes ?
  if (length(y) == length(x)){
    return(x)
  } else {
    #Facteur ou numerique ?
    if (is.factor(x) | is.character(x)){
      dist.freq = table(y) #distribution de frequences
      id.mode = which.max(dist.freq) #identifiant du mode
      z = x
      z[is.na(z)] = levels(z)[id.mode] #imputation par l'id du mode
      return(z)
    } else {
      moyenne = mean(y) #moyenne sur les valeurs restantes
      z = x
      z[is.na(z)] = moyenne #imputation par la moyenne
      return(z)
    }
  }
}

#'Function valeurs_manquantes_dataframe
#'
#'valeurs_manquantes_dataframe applies valeurs_manquantes to the dataframe
#'
#' @param X a dataframe cantaining the variables in the model
#'
#' @return Returns the new dataframe without missing values
#'
#' @export
valeurs_manquantes_dataframe = function(X){

  #Remplacer par la moyenne ou le mode les valeurs manquantes
  transformation = lapply(X,valeurs_manquantes)
  X = data.frame(transformation)

  #Supprimer les colonnes remplies de NA
  na = is.na(X)
  ind = unique(which(na,arr.ind=TRUE)[,2])
  if (length(ind)>0){
    X = X[,-ind]
  }

  #On retourne notre X sans valeurs manquantes
  return(X)
}
