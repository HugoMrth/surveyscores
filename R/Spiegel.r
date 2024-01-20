Spiegel <- function(x, scoring = FALSE) {

  x <- as.data.frame(x)

  if(dim(x)[2] != 6){
    stop("La matrice d'entrée doit contenir 6 colonnes pour les 6 questions utilisees pour la construction du score.")
  }
  if(any(apply(x, 2, function(x) {length(table(x))}) > 6)){
    stop("Au moins une des questions possede plus de 6 niveaux")
  }


  if (!scoring) {
    for (i in 1:ncol(x)) {
      x[, i] <- 6 - as.numeric(x[, i])
    }
  } else {
    for (i in 1:ncol(x)) {
      x[, i] <- as.numeric(x[, i])
    }
  }

  Score_Spiegel_num <- rowSums(x)

  Score_Spiegel_cl <- cut(Score_Spiegel_num,
                          breaks = c(-Inf, 14, 20, Inf),
                          labels = c("Sommeil pathologique",
                                     "Qualité de sommeil moyenne",
                                     "Bonne qualité de sommeil"))

  return(list(Valeur = Score_Spiegel_num,
              Classe = Score_Spiegel_cl))
}


# X <- cbind(sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE))
#
# Spiegel(X)
# table(Spiegel(X)$Classe)
