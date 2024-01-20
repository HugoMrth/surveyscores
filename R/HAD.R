HAD <- function(x,
                inv.anxiete = FALSE,
                inv.depression = FALSE) {


  x <- as.data.frame(x)

  if(dim(x)[2] != 14){
    stop("La matrice d'entrée doit contenir 14 colonnes pour les 14 questions utilisees pour la construction du score.")
  }
  if(any(apply(x, 2, function(x) {length(table(x))}) > 4)){
    stop("Au moins une des questions possede plus de 4 niveaux")
  }


  for (i in seq(1, 13, 2)) {
    if (inv.anxiete) {
      x[, i] <- 4 - as.numeric(x[, i])
    } else {
      x[, i] <- as.numeric(x[, i]) - 1
    }
  }
  for (i in seq(2, 14, 2)) {
    if (inv.depression) {
      x[, i] <- 4 - as.numeric(x[, i])
    } else {
      x[, i] <- as.numeric(x[, i]) - 1
    }
  }


  Score_HAD_Anxiete_num <- rowSums(sapply(x[, seq(1, 13, 2)], as.numeric), na.rm = TRUE)
  Score_HAD_Anxiete_cl <- cut(Score_HAD_Anxiete_num,
                              breaks = c(-Inf, 7, 10, Inf),
                              labels = c("Absence de symptomatologie anxieuse",
                                         "Symptomatologie anxieuse douteuse",
                                         "Symptomatologie anxieuse certaine"))


  Score_HAD_Depression_num <- rowSums(sapply(x[, seq(2, 14, 2)], as.numeric), na.rm = TRUE)
  Score_HAD_Depression_cl <- cut(Score_HAD_Depression_num,
                                 breaks = c(-Inf, 7, 10, Inf),
                                 labels = c("Absence de symptomatologie dépressive",
                                            "Symptomatologie dépressive douteuse ",
                                            "Symptomatologie dépressive certaine"))



  Score_HAD_num <- Score_HAD_Anxiete_num + Score_HAD_Depression_num
  Score_HAD_cl <- cut(Score_HAD_num,
                      breaks = c(-Inf, 18, Inf),
                      labels = c("Absence d'épisode majeur dépressif",
                                 "Présence d'un épisode majeur dépressif"))

  return(list(Valeur = data.frame(Anxiete = Score_HAD_Anxiete_num,
                              Depression = Score_HAD_Depression_num,
                              HAD = Score_HAD_num),
              Classe = data.frame(Anxiete = Score_HAD_Anxiete_cl,
                             Depression = Score_HAD_Depression_cl,
                             HAD = Score_HAD_cl)))
}
