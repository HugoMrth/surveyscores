Elixhauser.CI <- function(x, selection = FALSE) {

  x <- as.data.frame(x)
  if(dim(x)[2] != 21 & dim(x)[2] != 30) stop("La matrice d'entrée doit contenir 21 ou 30 colonnes pour la construction du score.")
  if(any(apply(x[, -1], 2, function(x) {length(table(x))}) > 2)) stop("Au moins une des questions possede plus de 2 niveaux alors que les réponses doivent etre binaires.")

  if (selection) {
    if(dim(x)[2] != 21) stop("La matrice d'entrée doit contenir 21 colonnes pour les 21 questions utilisees pour la construction du score.")
    Score_CCI_num <- 7*x[,1] + 5*x[,2] - 1*x[,3] + 4*x[,4] + 2*x[,5] + 7*x[,6] + 6*x[,7] + 3*x[,8]
    5*x[,9] + 11*x[,10] + 9*x[,11] + 12*x[,12] + 4*x[,13] + 3*x[,14] -
      4*x[,15] + 6*x[,16] + 5*x[,17] - 2*x[,18] - 2*x[,19] - 7*x[,20] - 3*x[,21]
  } else {
    if(dim(x)[2] != 30) stop("La matrice d'entrée doit contenir 30 colonnes pour les 30 questions utilisees pour la construction du score.")
    Score_CCI_num <- 7*x[,1] + 5*x[,2] - 1*x[,3] + 4*x[,4] + 2*x[,5] + 0*x[,6] + 7*x[,7] + 6*x[,8] + 3*x[,9] + 0*x[,10] + 0*x[,11] +
      0*x[,12] + 5*x[,13] + 11*x[,14] + 0*x[,15] + 0*x[,16] + 9*x[,17] + 12*x[,18] + 4*x[,19] + 0*x[,20] + 3*x[,21] -
      4*x[,22] + 6*x[,23] + 5*x[,24] - 2*x[,25] - 2*x[,26] + 0*x[,27] - 7*x[,28] + 0*x[,29] - 3*x[,30]
  }

  Prob_Survie_10ans <- round(0.983^(exp(Score_CCI_num*0.9))*100, 2)


  return(list(Score = Score_CCI_num,
              Prob_Survie_10ans = Prob_Survie_10ans))
}


