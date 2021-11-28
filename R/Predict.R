#'Function class_belonging
#'
#'class_belonging function allows to calculate the class of belonging : 1 if x>0 otherwise 0
#'
#' @param x it's a param
#'
#' @return Returns 0 or 1
#' @export
classe_appartenance <- function(x){
  if(x>0){
    return(1)
  } else {
    return(0)
  }
}



#'Function predict
#'
#'Predict function allows us to do our prediction
#'
#' @param objet Object is a Fit S3 object
#' @param  newdata a dataframe the same type we use in Fit
#' @param type Type is qualitative variable with two modalities : class and probability
#'
#' @return Returns a list of prediction or probability
#'
#' @export
predict <- function(objet,newdata,type){

  #                                                                                #
  # Verification de coherence et application des transformations faites dans le fit
  #                                                                                #

  #Verification si le type saisi est "class" ou "posterior"


  #Recuperation du fichier de donnees mis dans le fit
  data = objet$donnees

  #Variable qui verifie si les noms des colonnes du fichier de donnees du fit sont les
  #memes que celui du predict
  match = colnames(data) == colnames(newdata)

  #Verification si l'objet est bien l'objet S3 retourne dans le fit
  if (class(objet) != "Reg_Logistique"){
    stop("Argument inadequat : use a object of type Reg.Logistique")
  }

  #Verification de la coherence entre le nb de coefficients et le nb de colonnes newdata
  if (length(objet$coef) != ncol(newdata)){
    stop("Votre jeu de donnees ne correspond pas au jeu utilise dans le fit")
  }

  #          #
  # Fonction
  #          #

  #Si l'entree est bien un dataframe et que les noms des colonnes data et newdate matchent
  if ((is.data.frame(newdata)) & ((is.element(FALSE,match))==FALSE)){

    #Rajout d'une colonne de 1 pour le biais
    vec = matrix(1,ncol = 1,nrow = nrow(newdata))
    newdata = cbind(vec,newdata)

    #Valeurs manquantes
    if (sum(is.na(newdata))>0){
      newdata = valeurs_manquantes_dataframe(newdata)
    }

    #centrer-reduire le reste des variables sauf la premiere colonne remplie de 1
    newdata = centr_reduc_dataframe(newdata[,-1])


    #fonction dummies : recodage des variables qualitatives en 0/1
    if (sum(sapply(newdata,is.factor))>0){
      newdata = dummy_columns(as.data.frame(newdata),remove_selected_columns = TRUE)
    }

    #                                                       #
    # Calcul des probabilites et des classes d'appartenance
    #                                                       #

    newdata = cbind(rep(1,nrow(newdata)),newdata)

    #Transformation en matrice du newdata et en vecteur les coefficients
    newdata = as.matrix(newdata)
    coef = as.vector(objet$coef)

    #Classes d'appartenance
    C = newdata %*% coef
    y_pred = apply(C,1,classe_appartenance)

    #Probabilites
    prob = exp(C)/(1+exp(C))

    #                                                 #
    # Conditions pour retourner selon le type demande
    #                                                 #

    if (type=="class"){
      return(y_pred)
    }
    else {
      return(prob)
    }
  }
}
