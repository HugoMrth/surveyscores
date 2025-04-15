Charlson <- function(x) {
  x <- as.data.frame(x)
  if(dim(x)[2] != 20) stop("La matrice d'entrée doit contenir 20 colonnes pour les 20 questions utilisees pour la construction du score.")
  if(any(apply(x[, -1], 2, function(x) {length(table(x))}) > 2)) stop("Au moins une des questions possede plus de 2 niveaux alors que les réponses doivent etre binaires.")

  x[, 1] <- floor((x[, 1]-40)/10)
  x[, 1] <- ifelse(x[, 1] < 0, 0, ifelse(x[, 1] > 4, 4, x[, 1]))
  Score_CCI_num <- x[, 1] + rowSums(x[, 2:11]) + 2*rowSums(x[, 12:17]) + 3*x[, 18] + 6*rowSums(x[, 19:20])

  Prob_Survie_10ans <- round(0.983^(exp(Score_CCI_num*0.9))*100, 2)

  return(list(Score = Score_CCI_num,
              Prob_Survie_10ans = Prob_Survie_10ans))
}
