GHQ12 <- function(x, inv.scale = FALSE) {

  x <- as.data.frame(x)
  if(dim(x)[2] != 12) stop("La matrice d'entrée doit contenir 12 colonnes pour les 12 questions utilisees pour la construction du score.")
  if(any(apply(x, 2, function(x) {length(table(x))}) > 4)) stop("Au moins une des questions possede plus de 4 niveaux")
  if (inv.scale) x <- apply(x, 2, function(y) {5 - as.numeric(y)})

  x <- apply(x, 2, function(y) {ifelse(y <= 2, 0, 1)})

  Score_GHQ_num <- rowSums(x, na.rm = TRUE)
  Score_GHQ_cl <- cut(Score_GHQ_num,
                        breaks = c(-Inf, 3, 8, Inf),
                        labels = c("Absence de survenue de détresse psychologique",
                                   "Survenue d’une détresse psychologique",
                                   "Détresse psychologique sévère"))

  return(list(Valeur = Score_GHQ_num,
              Classe = Score_GHQ_cl))
}
