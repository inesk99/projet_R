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
#'LL it's a  log-loss function
#'
#' @param theta is gradient parametre
#' @param X  a  numeric matrix
#' @param y a numeric vector
#'
#' @return Returns cost value
#'
#' @export
#'
LL <- function(theta,X,y){

  m <- nrow(X)
  h <- prediction(X%*%theta)
  for(i in 1:nrow(h)){
    if(h[i,] == 1){
      h[i,] = 0.99999
    }
  }
  cost = (t(-y)%*%log(h) - t(1-y)%*%log(1-h))/m

  return(cost)
}

#'Function cost_derive
#'
#' cost_derive function is the derivation of the log-loss function
#'
#' @param theta is gradient parametre
#' @param X a numeric matrix
#' @param y a numeric vector
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
#'Gradient_stochastique function to calcul the simple gradient.
#'
#' @param X a numetric matrix
#' @param y a numeric vector
#' @param theta is a gradient coefficient
#' @param epsilon The parametre that tells us when we converge
#' @param learning_rate a double value
#' @param max_iter a integer
#' @param batch_size Select a sample of the dataframe(batch_size=1:online,batch_size=nrow(data):batch,1<batch_size<nrow(data):mini-batch )
#'
#' @return a vector theta which is descent gradient coefficients
#'
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
  plot(list_cout,type="l")

  #Affichage du nombre d'iteration
  print(paste("Nombre d'iterations :",iter))

  #On retourne theta : nos coefficients
  return(theta)
}

#'Function SGD_paralle
#'
#'SGD_paralle function is the same as gradient_stochastique function but with parallelisation
#'
#' @param data is a dataframe that containes X and y
#' @param theta is a gradient coefficient
#' @param k is the nubmer of cores to want use
#' @param epsilon The parametre that tells us when we converge
#' @param learning_rate a double value
#' @param max_iter an integer
#' @param batch_size Select a sample of the dataframe(batch_size=1:online,batch_size=nrow(data):batch,1<batch_size<nrow(data):mini-batch )
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



  #traité avec tout les coeurs
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
    globalVariables(names = i,package = "PackageRegLog",add = TRUE)

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
