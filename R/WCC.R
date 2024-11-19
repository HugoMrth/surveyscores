WCC <- function(x, sexe, label.femme,
                inv.scale = FALSE) {


  x <- as.data.frame(x)

  if(dim(x)[2] != 27) stop("La matrice d'entrée doit contenir 27 colonnes pour les 27 questions utilisees pour la construction du score.")
  if(any(apply(x, 2, function(x) {length(table(x))}) > 4)) stop("Au moins une des questions possede plus de 4 niveaux")

    if (inv.scale) {
      x <- apply(x, 2, function(y) {5 - as.numeric(y)})
    } else {
      x <- apply(x, 2, function(y) {as.numeric(y)})
    }


  score_wcc_prob <- apply(data.frame(x[, c(seq(1, 25, 3), 27)]), 1, sum, na.rm = FALSE)

  score_wcc_prob_cat <- score_wcc_prob

  score_wcc_prob_cat[score_wcc_prob<28.04 & sexe!=label.femme]<-1
  score_wcc_prob_cat[score_wcc_prob<27.79 & sexe==label.femme]<-1
  score_wcc_prob_cat[score_wcc_prob>=28.04 & sexe!=label.femme]<-2
  score_wcc_prob_cat[score_wcc_prob>=27.79 & sexe==label.femme]<-2

  score_wcc_prob_cat <- factor(score_wcc_prob_cat,
                               levels=1:2,
                               labels = c("Inférieur à la norme francaise",
                                          "Supérieur à la norme francaise"))


  #Sous score: Coping ?motion
  score_wcc_emotion <- apply(data.frame(x[, seq(2, 26, 3)]), 1, sum, na.rm = FALSE)

  score_wcc_emotion_cat <- score_wcc_emotion

  score_wcc_emotion_cat[score_wcc_emotion<20.22 & sexe!=label.femme]<-1
  score_wcc_emotion_cat[score_wcc_emotion<21.70 & sexe==label.femme]<-1
  score_wcc_emotion_cat[score_wcc_emotion>=20.22 & sexe!=label.femme]<-2
  score_wcc_emotion_cat[score_wcc_emotion>=21.70 & sexe==label.femme]<-2

  score_wcc_emotion_cat <- factor(score_wcc_emotion_cat,
                                  levels=1:2,
                                  labels = c("Inférieur à la norme francaise",
                                             "Supérieur à la norme francaise"))

  #Sous score: Recherche soutien
  score_wcc_soutien <- apply(data.frame(x[, seq(3, 24, 3)]), 1, sum, na.rm = FALSE)

  score_wcc_soutien_cat <- score_wcc_soutien

  score_wcc_soutien_cat[score_wcc_soutien<25.5 & sexe!=label.femme]<-1
  score_wcc_soutien_cat[score_wcc_soutien<20.2 & sexe==label.femme]<-1
  score_wcc_soutien_cat[score_wcc_soutien>=25.5 & sexe!=label.femme]<-2
  score_wcc_soutien_cat[score_wcc_soutien>=20.2 & sexe==label.femme]<-2

  score_wcc_soutien_cat <- factor(score_wcc_soutien_cat,
                                  levels = 1:2,
                                  labels = c("Inférieur à la norme francaise",
                                             "Supérieur à la norme francaise"))



  return(list(Valeur = data.frame(`Coping problème` = score_wcc_prob,
                                  `Coping émotion` = score_wcc_emotion,
                                  `Recherche soutien` = score_wcc_soutien),
              Classe = data.frame(`Coping problème` = score_wcc_prob_cat,
                                  `Coping émotion` = score_wcc_emotion_cat,
                                  `Recherche soutien` = score_wcc_soutien_cat)))
}
