#'Function classe_appartenance
#'
#'It's a function allows to calculate the class of belonging : 1 if x>0 otherwise 0
#'
#' @param x corresponds to the row of the calculation of LOGIT
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
#'Predict is a function that allows us to make our prediction
#'
#' @param objet Object is a Fit S3 object
#' @param  newdata a same dataframe that use in Fit, but without the target variable
#' @param type Type is qualitative variable with two modalities : class and posterior
#'
#' @return Returns a list of prediction (0/1) or probability
#'
#' @export
predict <- function(objet,newdata,type){

  #                                                                                #
  # Verification de coherence et application des transformations faites dans le fit
  #                                                                                #

  #Recuperation du fichier de donnees mis dans le fit
  data = objet$donnees

  #Variable qui verifie si les noms des colonnes du fichier de donnees du fit sont les
  #memes que celui du predict
  match = colnames(data) == colnames(newdata)

  #Verification si l'objet est bien l'objet S3 retourne dans le fit
  if (class(objet) != "Reg_Logistique"){
    stop("Inadequate argument : use a object of type Reg.Logistique")
  }

  #Verification de la coherence entre le nb de coefficients et le nb de colonnes newdata
  if (length(objet$coef) - 1 != ncol(newdata)){
    stop("Your data set does not match the set used in the fit")
  }

  #Verification si le type saisi est "class" ou "posterior"
  if(type != "class" & type != "posterior" ){
    stop("Inadequate argument: the value of the type is neither of class nor posterior")
  }

  #          #
  # Fonction
  #          #

  #Si l'entree est bien un dataframe et que les noms des colonnes data et newdate matchent
  if ((is.data.frame(newdata)) & ((is.element(FALSE,match))==FALSE)){

    #Valeurs manquantes
    if (sum(is.na(newdata))>0){
      newdata = valeurs_manquantes_dataframe(newdata)
    }

    #centrer-reduire le reste des variables sauf la premiere colonne remplie de 1
    for(i in 1:ncol(newdata)){
      if(is.numeric(newdata[,i])){

        newdata[,i] = (newdata[,i] - objet$moy[i])/objet$sd[i]
        print(objet$sd[i])
      }else{
        newdata[,i] = newdata[,i]
      }
    }


    print(class(newdata))
    print(newdata)
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
