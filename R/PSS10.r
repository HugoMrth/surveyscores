PSS10 <- function(x, inv.scale = FALSE) {

  x <- as.data.frame(x)

  if(dim(x)[2] != 10){
    stop("La matrice d'entrée doit contenir 10 colonnes pour les 10 questions utilisees pour la construction du score.")
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

  for (i in c(1, 2, 3, 6, 9, 10)) {
    x[, i] <- 5 - as.numeric(x[, i])
  }
  for (i in c(4, 5, 7, 8)) {
    x[, i] <- as.numeric(x[, i]) - 1
  }

  Score_PSS10_Helplessness_num <- rowSums(sapply(x[, c(1, 2, 3, 6, 9, 10)], as.numeric), na.rm = TRUE)
  Score_PSS10_SelfEfficacy_num <- rowSums(sapply(x[, c(4, 5, 7, 8)], as.numeric), na.rm = TRUE)


  Score_PSS10_num <- Score_PSS10_Helplessness_num + Score_PSS10_SelfEfficacy_num

  Score_PSS10_cl <- cut(Score_PSS10_num,
                        breaks = c(-Inf, 20, 26, Inf),
                        labels = c("Bonne capacité à gérer le stress",
                                   "Capacité à gérer le stress en général",
                                   "Situation de menace perpétuelle"))

  return(list(Valeur = data.frame(Helplessness = Score_PSS10_Helplessness_num,
                                  `Self Efficacy` = Score_PSS10_SelfEfficacy_num,
                                  PSS10 = Score_PSS10_num),
              Classe = Score_PSS10_cl))
}


# X <- cbind(sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE))
#
# PSS10(X)
# table(PSS10(X)$Classe)
