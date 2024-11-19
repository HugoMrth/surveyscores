WEMWBS <- function(x, inv.scale = FALSE) {

  x <- as.data.frame(x)

  if(dim(x)[2] != 14) stop("La matrice d'entrée doit contenir 14 colonnes pour les 14 questions utilisees pour la construction du score.")
  if(any(apply(x, 2, function(x) {length(table(x))}) > 5)) stop("Au moins une des questions possede plus de 5 niveaux")


  if (inv.scale) {
    x <- apply(x, 2, function(y) {6 - as.numeric(y)})
  }
  x <- apply(x, 2, function(y) {as.numeric(y)})

  Score_WWE_num <- rowSums(x)
  return(Score_WWE_num)
}
