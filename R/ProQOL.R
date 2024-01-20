ProQOL <- function(x,
                inv.scale = FALSE) {


  x <- as.data.frame(x)

  if(dim(x)[2] != 30){
    stop("La matrice d'entrée doit contenir 30 colonnes pour les 30 questions utilisees pour la construction du score.")
  }
  if(any(apply(x, 2, function(x) {length(table(x))}) > 5)){
    stop("Au moins une des questions possede plus de 5 niveaux")
  }


  if (inv.scale) {
    for (i in 1:ncol(x)) {
      x[, i] <- 6 - as.numeric(x[, i])
    }
  }


  # PROQOL - Satisfaction de la compassion ####

  Score_PROQOL_Compassion_num <- rowSums(sapply(x[, c(3, 6, 12,
                                                              16, 18, 20,
                                                              22, 24, 27,
                                                              30)], as.numeric), na.rm = TRUE)

  Score_PROQOL_Compassion_cl <- cut(Score_PROQOL_Compassion_num,
                                         breaks = c(-Inf, 22, 41, Inf),
                                         labels = c("Faible",
                                                    "Moyen",
                                                    "Eleve"))

  # PROQOL - Épuisement professionnel ####

  Score_PROQOL_Epuissement_num <- rowSums(sapply(x[, c(8, 10, 19, 21, 26)], as.numeric), na.rm = TRUE) +
    #Inverse : 6 -
    rowSums(6 - sapply(x[, c(1, 4, 15, 17, 29)], as.numeric), na.rm = TRUE)

  Score_PROQOL_Epuissement_cl <- cut(Score_PROQOL_Epuissement_num,
                                          breaks = c(-Inf, 22, 41, Inf),
                                          labels = c("Faible",
                                                     "Moyen",
                                                     "Eleve"))

  # PROQOL - Stress traumatique secondaire ####

  Score_PROQOL_Stress_num <- rowSums(sapply(x[, c(2, 5, 7,
                                                          9, 11, 13,
                                                          14, 23, 25,
                                                          23)], as.numeric), na.rm = TRUE)

  Score_PROQOL_Stress_cl <- cut(Score_PROQOL_Stress_num,
                                     breaks = c(-Inf, 22, 41, Inf),
                                     labels = c("Faible",
                                                "Moyen",
                                                "Eleve"))

  return(list(Valeur = data.frame(Compassion = Score_PROQOL_Compassion_num,
                              Epuissement = Score_PROQOL_Epuissement_num,
                              Stress = Score_PROQOL_Stress_num),
              Classe = data.frame(Compassion = Score_PROQOL_Compassion_cl,
                                  Epuissement = Score_PROQOL_Epuissement_cl,
                                  Stress = Score_PROQOL_Stress_cl)))
}


# X <- matrix(data = sample.int(5, size = 50*30, replace = TRUE),
#             ncol = 30)
#
# ProQOL(X)
# apply(ProQOL(X)$Classe, 2, table)
