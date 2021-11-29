#'Function prediction
#'
#'prediction function allows to calculate the sigmoid of a value
#'
#' @param z it's matrix calculation between X and theta
#'
#' @return Returns a vector
#' @export
prediction <- function(z){
  return(1/(1+exp(-z)))
}

#'Function LL
#'
#'LL it's a  log-loss function of logistic regression
#'
#' @param theta is gradient parameter
#' @param X  a  numeric matrix of features
#' @param y a numeric vector to target variable
#'
#' @return Returns cost value
#'
#' @export
LL <- function(theta,X,y){

  m <- nrow(X)
  h <- prediction(X%*%theta)
  h <- replace(h, h==1,0.9999)
  cost = (t(-y)%*%log(h) - t(1-y)%*%log(1-h))/m

  return(cost)
}

#'Function cost_derive
#'
#' cost_derive function is the derivation of the log-loss function
#'
#' @param theta is gradient parameter
#' @param X a  numeric matrix of features
#' @param y a numeric matrix to target variable
#'
#' @return Returns gradient
#'
#' @export
cost_derive <- function(theta,X,y){

  m <- nrow(X)
  h <- prediction(X%*%theta)
  deriver <- (t(X)%*%(h - y))/m

  return(as.vector(deriver))
}

#'Function gradient_stochastique
#'
#'It's a function to calculate the stochasitic gradient descent.
#'Batch size corresponds to the sampling mode. If it's equal to 1 then we are doing online. Included between 1 and number of lines, it's a mini batch.
#'And if it's equal to the number of lines then it's a batch.
#'
#' @param X a numeric matrix of features
#' @param y a numeric matrix to target variable
#' @param theta a numeric vector.It includes the same number of data as the column number of X. It is initiated randomly. Theta allows us to calculate the gradients.
#' @param epsilon a numerical value that allows us to stop when calculating gradients.
#' @param learning_rate  numeric value which determines the speed of convergence of the process
#' @param max_iter a numeric value to allows stop the gradient descent if we don't converge.
#' @param batch_size a number to allow selection of a sample from the database
#'
#' @importFrom graphics title
#'
#' @return a vector theta which is descent gradient coefficients
#'
#' @export
gradient_stochastique <- function(X,y,theta,epsilon,learning_rate,max_iter,batch_size){

  #Conditions d'arrets
  iter <- 0
  converge <- FALSE

  #Liste de la fonction de cout
  list_cout = c()

  #Tant que l'iteration est inferieure au max d'iteration et que converge reste a FALSE
  while((iter < max_iter) && (converge == FALSE)){

    #On incremente de 1 l'interaction
    iter = iter + 1

    #                     #
    #  Variables X et y
    #                     #

    #Selection des X et y en fonction du batch_size de maniere aleatoire
    ind = sample(nrow(X),batch_size,replace = TRUE)
    X_batch = X[ind,]
    y_batch = y[ind,]

    #Transformation en matrice des X_batch et y_batch
    X_batch = as.matrix(X_batch)
    y_batch = as.matrix(y_batch)

    #Si on est dans un online, on transpose la X_batch
    if(batch_size == 1){
      X_batch = t(X_batch)
    }

    #                                                   #
    #  Fonction de cout et mise a jour des coefficients
    #                                                   #

    #Calcul de la fonction de cout
    cost = LL(theta, X_batch,y_batch)
    list_cout = c(list_cout,cost)

    #Calcul des derivees partielles de fonction cout (gradient)
    #Donne les coeficients directeurs de la droite
    g = cost_derive(theta,X_batch,y_batch)

    #Calcul qui permet de mettre a jour les thetas
    thetabis = theta - learning_rate * g

    #              #
    #  Convergence
    #              #

    if(sum(abs(thetabis-theta))< epsilon){
      converge = TRUE
    }
    theta = thetabis

  }

  #Affichage du graphique de la fonction de cout
  plot(list_cout,type="l",xlab="Number of iterations",ylab="Cost")
  title(main = "cost function")


  #Affichage du nombre d'iteration
  print(paste("Number of iterations :",iter))

  #On retourne theta : nos coefficients
  return(theta)
}

#'Function SGD_paralle
#'
#'SGD_paralle function is the same as gradient_stochastique function but with parallelisation
#'Batch size corresponds to the sampling mode. If it's equal to 1 then we are doing online. Included between 1 and number of lines, it's a mini batch.
#'And if it's equal to the number of lines then it's a batch.
#'
#' @param data a numeric matrix of features
#' @param theta a numeric vector.It includes the same number of data as the column number of X. It is initiated randomly. Theta allows us to calculate the gradients.
#' @param k  allows to split our data set according to the hearts which were entered.
#' @param epsilon a numerical value that allows us to stop when calculating gradients.
#' @param learning_rate  numeric value which determines the speed of convergence of the process
#' @param max_iter a numeric value to allows stop the gradient descent if we don't converge.
#' @param batch_size a number to allow selection of a sample from the database
#'
#' @import doParallel
#' @import parallel
#' @import foreach
#' @importFrom utils globalVariables
#'
#'
#' @return a combination of k descent gradient vectors
#'
#' @export
SGD_paralle <- function(data,theta,k,epsilon,learning_rate,max_iter,batch_size){



  #traité avec tous les coeurs
  if(k == -1){
    k = parallel::detectCores() - 1
  }
  #Separation du jeu de donnees en blocs : nb de blocs k
  blocs = split(data,1+(1:nrow(data))%%k)

  #Ouverture et creation des clusters
  cl = parallel::makeCluster(k)

  #Programmation parallele
  doParallel::registerDoParallel(cl)

  #Pour chaque bloc...
  res = foreach::foreach(i= blocs,.combine = 'cbind',.export = c('gradient_stochastique','LL','prediction','cost_derive')) %dopar% {
    # gestion de i commme une variable global

    n = ncol(i)
    X = as.matrix(i[,-n])
    y = as.matrix(i[,n])

    SGD = gradient_stochastique(X,y,theta,epsilon,learning_rate,max_iter,batch_size)
    return(SGD)
  }

  #Arret de nos clusters/coeurs
  parallel::stopCluster(cl)

  #On retourne le resultat
  return(res)
}
