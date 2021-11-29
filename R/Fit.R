#'Function Fit
#'
#'Fit function allows us to calculate the coefficients with a Stochastic gradient descent.
#'Batch size corresponds to the sampling mode. If it's equal to 1 then we are doing online. Included between 1 and number of lines, it's a mini batch.
#'And if it's equal to the number of lines then it's a batch.
#'
#' @param formula an object of class formula. allows you to select the target variable and features variables.
#' @param data a dataframe cantaining the variables in the model.
#' @param epsilon  a numeric value to allows us to converge and stop the descent gradient
#' @param learning_rate a numeric value which determines the speed of convergence of the process
#' @param max_iter a numeric value to allows stop the gradient descent if we don't converge.
#' @param batch_size  a number to allow selection of a sample from the database
#' @param ncores a numeric value to determine the number of hearts to use
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
fit <- function(formula,data,epsilon = 1e-4,learning_rate = 0.01,max_iter = 1500,batch_size= 200,ncores=0){


  #On teste si les entrees sont bien une formule et un dataframe
  if(rlang::is_formula(formula) && (is.data.frame(data))){

    #Recuperation du y et les x
    vars = stats::get_all_vars(formula, data)
    y = vars[1]
    x = vars[,-1]

    #verification que y soit facteur
    if(is.factor(y[,1])== FALSE && is.numeric(y[,1])== FALSE){
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
    moy = unlist(lapply(x[1:ncol(x)], save.mean))
    sd = unlist(lapply(x[1:ncol(x)], save.sd))

    moy = as.vector(moy)
    sd = as.vector(sd)

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
    prob = replace(prob,prob==1,0.99999)

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
    instance$moy <- moy
    instance$sd <- sd
    instance$ddl <- nrow(data)-ncol(data)
    instance$deviance <- ll
    class(instance) <- "Reg_Logistique"
    return(instance)

  }
}
