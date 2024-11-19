MBI <- function(x, inv.scale = FALSE) {
  x <- as.data.frame(x)
  if(dim(x)[2] != 22) stop("La matrice d'entrée doit contenir 22 colonnes pour les 22 questions utilisees pour la construction du score.")
  if(any(apply(x, 2, function(x) {length(table(x))}) > 7)) stop("Au moins une des questions possede plus de 7 niveaux")


  if (inv.scale) {
    x <- apply(x, 2, function(y) {
      7 - as.numeric(y)
    })
  } else {
    x <- apply(x, 2, function(y) {
      as.numeric(y) - 1
    })
  }

  # Epuisement Professionnel
  score_mbi_sep <- apply(data.frame(x[, c(1, 2, 3, 6, 8, 13, 14, 16, 20)]), 1, sum, na.rm = FALSE)

  score_mbi_sep_cat <- score_mbi_sep

  score_mbi_sep_cat[score_mbi_sep<=17] <- 1
  score_mbi_sep_cat[score_mbi_sep>17 & score_mbi_sep<=29] <- 2
  score_mbi_sep_cat[score_mbi_sep>=30] <- 3

  score_mbi_sep_cat<-factor(score_mbi_sep_cat,
                            levels = 1:3,
                            labels = c("Degré faible d'épuisement professionnel",
                                       "Degré moyen d'épuisement professionnel",
                                       "Degré élevé d'épuisement professionnel"))



  #D?personnalisation / Perte d'empathie (SD)
  score_mbi_sd <- apply(data.frame(x[, c(5, 10, 11, 15, 22)]), 1, sum, na.rm = FALSE)

  score_mbi_sd_cat <- score_mbi_sd

  score_mbi_sd_cat[score_mbi_sd<=5] <- 1
  score_mbi_sd_cat[score_mbi_sd>5 & score_mbi_sd<=11] <- 2
  score_mbi_sd_cat[score_mbi_sd>=12] <- 3
  score_mbi_sd_cat<-factor(score_mbi_sd_cat,
                           levels = 1:3,
                           labels = c("Degré faible de perte d'empathie",
                                      "Degré moyen de perte d'empathie",
                                      "Degré élevé de perte d'empathie"))


  #Accomplissement Personnel (SAP)
  score_mbi_sap <- apply(data.frame(x[, c(4, 7, 9, 12, 17, 18, 19, 21)]), 1, sum, na.rm = FALSE)

  score_mbi_sap_cat <- score_mbi_sap

  score_mbi_sap_cat[score_mbi_sap<=33] <- 1
  score_mbi_sap_cat[score_mbi_sap>33 & score_mbi_sap<=39] <- 2
  score_mbi_sap_cat[score_mbi_sap>=40] <- 3
  score_mbi_sap_cat<-factor(score_mbi_sap_cat,
                            levels = 1:3,
                            labels = c("Degré faible d'accomplissement personnel",
                                       "Degré moyen d'accomplissement personnel",
                                       "Degré élevé d'accomplissement personnel"))


  return(list(Valeur = data.frame(`Epuisement Professionnel` = score_mbi_sep,
                                  `Depersonnalisation` = score_mbi_sd,
                                  `Accomplissement Personnel` = score_mbi_sap),
              Classe = data.frame(`Coping problème` = score_mbi_sep_cat,
                                  `Depersonnalisation` = score_mbi_sd_cat,
                                  `Accomplissement Personnel` = score_mbi_sap_cat)))
}
