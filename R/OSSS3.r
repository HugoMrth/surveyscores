OSSS3 <- function(x, inv.scale = FALSE) {

  x <- as.data.frame(x)

  if(dim(x)[2] != 3) stop("La matrice d'entrée doit contenir 3 colonnes pour les 3 questions utilisees pour la construction du score.")
  if(any(apply(x, 2, function(x) {length(table(x))}) > 5)) stop("Au moins une des questions possede plus de 5 niveaux")


  if (!inv.scale) {
    x[, 1] <- as.numeric(x[, 1])
    x[, 2] <- as.numeric(x[, 2])
    x[, 3] <- as.numeric(x[, 3])
  } else {
    x[, 1] <- 5 - as.numeric(x[, 1])
    x[, 2] <- 6 - as.numeric(x[, 2])
    x[, 3] <- 6 - as.numeric(x[, 3])
  }

  Score_OSSS_num <- rowSums(x)

  Score_OSSS_cl <- cut(Score_OSSS_num,
                       breaks = c(-Inf, 8, 11, Inf),
                       labels = c("Soutien social faible",
                                  "Soutien social modéré",
                                  "Soutien social élevé"))

  return(list(Valeur = Score_OSSS_num,
              Classe = Score_OSSS_cl))
}
