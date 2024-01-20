Glasgow <- function(x, inv.scale = FALSE) {

  x <- as.data.frame(x)

  if(dim(x)[2] != 3){
    stop("La matrice d'entrée doit contenir 3 colonnes pour les 3 questions utilisees pour la construction du score.")
  }
  if(length(table(x[,1])) > 4){
    stop("L'echelle d'ouverture des yeux est en plus de 4 niveaux")
  }
  if(length(table(x[,2])) > 5){
    stop("L'echelle de reponse verbale est en plus de 5 niveaux")
  }
  if(length(table(x[,3])) > 6){
    stop("L'echelle de reponse motrice est en plus de 6 niveaux")
  }

  if (inv.scale) {
    Score_Glasgow_num <- (5 - as.numeric(x[,1])) + (6 - as.numeric(x[,2])) + (7 - as.numeric(x[,3]))
  } else {
    Score_Glasgow_num <- as.numeric(x[,1]) + as.numeric(x[,2]) + as.numeric(x[,3])
  }

Score_Glasgow_cl <- cut(Score_Glasgow_num,
                         breaks = c(-Inf, 6, 9, 14, Inf),
                         labels = c("Coma profond ou mort clinique",
                                    "Coma lourd",
                                    "Somnolence ou coma léger",
                                    "Conscience normale"))

  return(list(Valeur = Score_Glasgow_num,
              Classe = Score_Glasgow_cl))
}


# X <- cbind(sample.int(4, size = 50, replace = TRUE),
#            sample.int(5, size = 50, replace = TRUE),
#            sample.int(6, size = 50, replace = TRUE))
#
# Glasgow(X)
