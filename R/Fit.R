#'Function Fit
#'
#'Fit function allows us to calculate the coefficients with a Stochastic gradient descent.
#'
#' @param formula Formula helps us to select the target variable and features
#' @param data select a Dataframe
#' @param epsilon  parameter that tells us when we converge
#' @param learning_rate a double value
#' @param max_iter an integer
#' @param batch_size Select a sample of the dataframe(batch_size=1:online ; batch_size=nrow(data):batch ; 1<batch_size<nrow(data):mini-batch )
#' @param ncores Number of processors
#'
#' @import dplyr
#' @importFrom fastDummies dummy_columns
#' @import rlang
#' @importFrom stats get_all_vars na.omit runif sd
#' @import usethis
#'
#' @return S3 object type Reg_Logistique
#'
#' @export
fit <- function(formula,data,epsilon = 1e-4,learning_rate = 0.01,max_iter = 1000,batch_size=1,ncores){


  #On teste si les entrees sont bien une formule et un dataframe
  if(rlang::is_formula(formula) && (is.data.frame(data))){

    #Recuperation du y et les x
    vars = stats::get_all_vars(formula, data)
    y = vars[1]
    x = vars[,-1]

    #verification que y soit facteur
    if(is.factor(y[,1])== FALSE){
      stop("y must be is factor !")
    }

    #Stockage dans une variable les variables explicatives non transformees
    verif = x

    #Transformation en df quand on selectionne une seule variable
    #Pour eviter que ca devienne un vecteur
    x = as.data.frame(x)

    #                               #
    # Verification des variables x
    #                               #

    #Valeurs manquantes :
    if (sum(is.na(x))>0){

      x = valeurs_manquantes_dataframe(x)
    }

    #Centrer-reduire le reste des variables sauf la premiere colonne remplie de 1
    x = centr_reduc_dataframe(x)

    #Transformation du x en dataframe
    x = as.data.frame(x)


    #Fonction dummies : recodage des variables qualitatives en 0/1
    if (sum(sapply(x,is.factor)) >0){
      x = fastDummies::dummy_columns(as.data.frame(x),remove_selected_columns = TRUE)
    }

    #Rajout d'une colonne de 1 pour le biais
    vec = matrix(1,ncol = 1,nrow = nrow(data))
    x = cbind(vec,x)

    #                                          #
    #  Recodage de la variable cible y en 0/1
    #                                          #

    if (is.factor(y[,1])){
      y = fastDummies::dummy_columns(as.data.frame(y),remove_first_dummy = TRUE,remove_selected_columns = TRUE)
    }


    #                                                       #
    #  Calcul des coefficients avec la descente de gradient
    #                                                       #

    #Transformation des x et y en matrice
    x = as.matrix(x)
    y = as.matrix(y)

    #Regroupement des x et du y pour former le dataframe total
    Xy = as.data.frame(cbind(x,y))

    #On genere des coefficients aleatoires
    theta = stats::runif(ncol(x))

    if(batch_size == nrow(x) && ncores>0){

      gradient = SGD_paralle(Xy,theta,ncores,epsilon,learning_rate,max_iter,batch_size)

      if(ncores > 1){
        gradient = apply(gradient, 1,mean)
      }
    } else {
      gradient = gradient_stochastique(x,y,theta,epsilon,learning_rate,max_iter,batch_size)
    }


    #                           #
    #  Calcul des probabilites
    #                           #

    C = x %*% gradient
    prob = exp(C)/(1+exp(C))

    #Gestion des valeurs 1 dans les probabilites
    replace(prob,prob==1,0.99999)

    ll = (y*log(prob)+(1-y)*log(1-prob))

    #                         #
    #  Retourner un objet S3 :
    #                         #

    call = function(x){
      var = substitute(x)
      return(var)
    }

    instance <- list()
    instance$donnees = verif
    instance$call <- paste(c("fit(",formula,",",call(data), ")"), sep = " ")
    instance$coef <- gradient
    instance$ddl <- nrow(data)-ncol(data)
    instance$deviance <- ll
    class(instance) <- "Reg_Logistique"
    return(instance)

  }
}
