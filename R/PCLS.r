PCLS <- function(x, inv.scale = FALSE) {
  x <- as.data.frame(x)
  if(dim(x)[2] != 17) stop("La matrice d'entrée doit contenir 17 colonnes pour les 17 questions utilisees pour la construction du score.")
  if(any(apply(x, 2, function(x) {length(table(x))}) > 5)) stop("Au moins une des questions possede plus de 5 niveaux")

  Score_PCLS_num <- rowSums(sapply(x, as.numeric))

  Score_PCLS_cl <- cut(Score_PCLS_num,
                            breaks = c(-Inf, 33, 43, Inf),
                            labels = c("Pas de symptomatologie significative",
                                       "Symptomatologie relevant d’une prise en charge psychiatrique sans ESPT franc",
                                       "Etat de stress post traumatique"))


  return(list(Valeur = Score_PCLS_num,
              Classe = Score_PCLS_cl))
}
