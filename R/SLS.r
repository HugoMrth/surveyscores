SLS <- function(x, inv.scale = FALSE) {

  x <- as.data.frame(x)

  if(dim(x)[2] != 3){
    stop("La matrice d'entrée doit contenir 3 colonnes pour les 3 questions utilisees pour la construction du score.")
  }
  if(any(apply(x, 2, function(x) {length(table(x))}) > 4)){
    stop("Au moins une des questions possede plus de 4 niveaux")
  }


  if (!inv.scale) {
    x[, 1] <- as.numeric(x[, 1])
    x[, 2] <- as.numeric(x[, 2])
    x[, 3] <- as.numeric(x[, 3])
  } else {
    x[, 1] <- 5 - as.numeric(x[, 1])
    x[, 2] <- 5 - as.numeric(x[, 2])
    x[, 3] <- 5 - as.numeric(x[, 3])
  }

  Score_SLS_num <- rowSums(x)


  return(Valeur = Score_SLS_num)
}


# X <- cbind(sample.int(4, size = 50, replace = TRUE),
#            sample.int(4, size = 50, replace = TRUE),
#            sample.int(4, size = 50, replace = TRUE))
#
# SLS(X)
