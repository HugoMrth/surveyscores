CAST <- function(x, inv.scale = FALSE) {

  x <- as.data.frame(x)

  if(dim(x)[2] != 6){
    stop("La matrice d'entrée doit contenir 6 colonnes pour les 6 questions utilisees pour la construction du score.")
  }
  if(any(apply(x, 2, function(x) {length(table(x))}) > 5)){
    stop("Au moins une des questions possede plus de 5 niveaux")
  }

  x <- as.data.frame(x)

  if (inv.scale) {
    for (i in 1:ncol(x)) {
      x[, i] <- 6 - as.numeric(x[, i])
    }
  }

  Score_CAST_num <- as.numeric(x[, 1]) - 1 +
    as.numeric(x[, 2]) - 1 +
    as.numeric(x[, 3]) - 1 +
    as.numeric(x[, 4]) - 1 +
    as.numeric(x[, 5]) - 1 +
    as.numeric(x[, 6]) - 1

  Score_CAST_cl <- cut(Score_CAST_num,
                       breaks = c(-Inf, 2, 6, Inf),
                       labels = c("Pas de risque de dépendance au cannabis",
                                  "Risque faible de dépendance au cannabis",
                                  "Risque élevé de dépendance au cannabis"))


  return(list(Valeur = Score_CAST_num,
              Classe = Score_CAST_cl))
}


# X <- cbind(sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE))
#
# CAST(X)
# table(CAST(X)$Classe)
