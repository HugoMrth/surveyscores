WEMWBS <- function(x, inv.scale = FALSE) {

  x <- as.data.frame(x)

  if(dim(x)[2] != 14){
    stop("La matrice d'entrée doit contenir 14 colonnes pour les 14 questions utilisees pour la construction du score.")
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

  for (i in 1:ncol(x)) {
    x[, i] <- as.numeric(x[, i])
  }

  Score_WWE_num <- rowSums(x)

  return(Score_WWE_num)
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
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE))
#
# WEMWBS(X)
# quantile(WEMWBS(X), c(0.25, 0.75))
