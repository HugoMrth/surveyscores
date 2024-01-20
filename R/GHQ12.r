GHQ12 <- function(x, inv.scale = FALSE) {

  x <- as.data.frame(x)

  if(dim(x)[2] != 12){
    stop("La matrice d'entrée doit contenir 12 colonnes pour les 12 questions utilisees pour la construction du score.")
  }
  if(any(apply(x, 2, function(x) {length(table(x))}) > 4)){
    stop("Au moins une des questions possede plus de 4 niveaux")
  }

  x <- as.data.frame(x)

  if (inv.scale) {
    for (i in 1:ncol(x)) {
      x[, i] <- 5 - as.numeric(x[, i])
    }
  }

  for (i in 1:ncol(x)) {
    x[x[, i] <= 2, i] <- 0
    x[x[, i] > 2, i] <- 1
  }

  Score_GHQ_num <- rowSums(sapply(x, as.numeric), na.rm = TRUE)

  Score_GHQ_cl <- cut(Score_GHQ_num,
                        breaks = c(-Inf, 3, 8, Inf),
                        labels = c("Absence de survenue de détresse psychologique",
                                   "Survenue d’une détresse psychologique",
                                   "Détresse psychologique sévère"))

  return(list(Valeur = Score_GHQ_num,
              Classe = Score_GHQ_cl))
}


# X <- cbind(sample.int(4, size = 50, replace = TRUE),
#            sample.int(4, size = 50, replace = TRUE),
#            sample.int(4, size = 50, replace = TRUE),
#            sample.int(4, size = 50, replace = TRUE),
#            sample.int(4, size = 50, replace = TRUE),
#            sample.int(4, size = 50, replace = TRUE),
#            sample.int(4, size = 50, replace = TRUE),
#            sample.int(4, size = 50, replace = TRUE),
#            sample.int(4, size = 50, replace = TRUE),
#            sample.int(4, size = 50, replace = TRUE),
#            sample.int(4, size = 50, replace = TRUE),
#            sample.int(4, size = 50, replace = TRUE))
#
# GHQ12(X)
# table(GHQ12(X)$Classe)
