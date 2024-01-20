HSI <- function(x, ind.NA = NULL) {

  x <- as.data.frame(x)

  if(dim(x)[2] != 2){
    stop("La matrice d'entrée doit contenir 2 colonnes pour les 2 questions utilisees pour la construction du score.")
  }
  if(any(apply(x, 2, function(x) {length(table(x))}) > 5)){
    stop("Au moins une des questions possede plus de 4 niveaux (sans compter les non fumeurs")
  }

  x[, 1] <- as.numeric(x[, 1]) - 2
  x[, 2] <- as.numeric(x[, 2]) - 2

  x[x[, 1] < 0, 1] <- 0
  x[x[, 2] < 0, 2] <- 0

  Score_HSI_num <- rowSums(x)

  if(!is.null(ind.NA)) {
    Score_HSI_num[ind.NA] <- NA
  }

  Score_HSI_cl <- cut(Score_HSI_num,
                           breaks = c(-Inf, 2, 4, Inf),
                           labels = c("Faible dépendance au tabac",
                                      "Moyenne dépendance au tabac",
                                      "Forte dépendance au tabac"))


  return(list(Valeur = Score_HSI_num,
              Classe = Score_HSI_cl))
}


# X <- cbind(sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE))
#
# non_fumeurs <- sample.int(100, size = 50, replace = TRUE) > 80
#
# HSI(X)
# HSI(X, ind.NA = non_fumeurs)


