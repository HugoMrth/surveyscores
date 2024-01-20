AUDITC <- function(x, sexe, label.femme = 0) {

  x <- as.data.frame(x)

  if(dim(x)[2] != 3){
    stop("La matrice d'entrée doit contenir 3 colonnes pour les 3 questions utilisees pour la construction du score.")
  }
  if(any(apply(x, 2, function(x) {length(table(x))}) > 6)){
    stop("Au moins une des questions possede plus de 6 niveaux")
  }

  Score_AUDITC_num <- as.numeric(x[,1]) - 1 +
    as.numeric(x[,2]) - 1 +
    as.numeric(x[,3]) - 1

  Score_AUDITC_num[sexe == label.femme & Score_AUDITC_num >= 3 & !is.na(Score_AUDITC_num) & !is.na(sexe)] <-
    Score_AUDITC_num[sexe == label.femme & Score_AUDITC_num >= 3 & !is.na(Score_AUDITC_num) & !is.na(sexe)] + 1

  Score_AUDITC_cl <- cut(Score_AUDITC_num,
                         breaks = c(-Inf, 0, 3, Inf),
                         labels = c("Pas de consommation d'alcool",
                                    "Consommation d'alcool non risquée pour la santé",
                                    "Consommation d'alcool à risque pour la santé"))

  return(list(Valeur = Score_AUDITC_num,
              Classe = Score_AUDITC_cl))
}


# X <- cbind(sample.int(5, size = 50, replace = TRUE),
#            sample.int(6, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE))
#
# sexe <- sample.int(2, size = 50, replace = TRUE)
#
# AUDITC(X, sexe, label.femme = 1)
