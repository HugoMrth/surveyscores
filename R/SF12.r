SF12 <- function(x) {

  if(dim(x)[2] != 12){
    stop("La matrice d'entrée doit contenir 12 colonnes pour les 12 questions utilisees pour la construction du score.")
  }


  x <- as.data.frame(x)
  DATA_SF12_Physique <- DATA_SF12_Mental <- x

  levels(DATA_SF12_Physique[, 1]) <- c(-1.31872, -3.02396, -5.56461, -8.37399)
  levels(DATA_SF12_Mental[, 1]) <- c(-0.06064, 0.03482, -0.16891, -1.71175)
  levels(DATA_SF12_Physique[, 2]) <- c(-7.23216, -3.45555, 0)
  levels(DATA_SF12_Mental[, 2]) <- c(3.93115, 1.86840, 0)
  levels(DATA_SF12_Physique[, 3]) <- c(-6.24397, -2.73557, 0)
  levels(DATA_SF12_Mental[, 3]) <- c(2.68282, 1.43103, 0)
  levels(DATA_SF12_Physique[, 4]) <- c(-4.61617, 0)
  levels(DATA_SF12_Mental[, 4]) <- c(1.44060, 0)
  levels(DATA_SF12_Physique[, 5]) <- c(-5.51747, 0)
  levels(DATA_SF12_Mental[, 5]) <- c(1.66968, 0)
  levels(DATA_SF12_Physique[, 6]) <- c(3.04365, 0)
  levels(DATA_SF12_Mental[, 6]) <- c(-6.82672, 0)
  levels(DATA_SF12_Physique[, 7]) <- c(2.32091, 0)
  levels(DATA_SF12_Mental[, 7]) <- c(-5.69921, 0)
  levels(DATA_SF12_Physique[, 8]) <- c(0, -3.80130, -6.50522, -8.38063, -11.25544)
  levels(DATA_SF12_Mental[, 8]) <- c(0, 0.90384, 1.49384, 1.76691, 1.48619)
  levels(DATA_SF12_Physique[, 9]) <- c(0.66514, 1.36689, 2.37241, 2.90426, 3.46638)
  levels(DATA_SF12_Mental[, 9]) <- c(-1.94949, -4.09842, -6.31121, -7.92717, -10.19085)
  levels(DATA_SF12_Physique[, 10]) <- c(-0.42251, -1.14387, -1.61850, -2.02168, -2.44706)
  levels(DATA_SF12_Mental[, 10]) <- c(-0.92057, -1.65178, -3.29805, -4.88962, -6.02409)
  levels(DATA_SF12_Physique[, 11]) <- c(0.41188, 1.280444, 2.34247, 3.41593, 4.61446)
  levels(DATA_SF12_Mental[, 11]) <- c(-1.95934, -4.59055, -8.09914, -10.77911, -16.15395)
  levels(DATA_SF12_Physique[, 12]) <- c(-0.33682, -0.94342, -0.18043, 0.11038, 0)
  levels(DATA_SF12_Mental[, 12]) <- c(-6.29724, -8.26066, -5.63286, -3.13896, 0)

  Score_PCS_num <- 56.57706 + rowSums(apply(DATA_SF12_Physique, 2, as.numeric))
  Score_MCS_num <- 60.75781 + rowSums(apply(DATA_SF12_Mental, 2, as.numeric))

  Score_PCS_cl <- cut(Score_PCS_num,
                      breaks = c(-Inf, 46.53, 56.49, Inf),
                      labels = c("Mauvaise Santé Physique",
                                 "Santé Physique Moyenne",
                                 "Bonne Santé Physique"))

  Score_MCS_cl <- cut(Score_MCS_num,
                      breaks = c(-Inf, 45.13, 57.30, Inf),
                      labels = c("Mauvaise Santé Mentale",
                                 "Santé Mentale Moyenne",
                                 "Bonne Santé Mentale"))

  return(list(Valeur = data.frame(Physique = Score_PCS_num,
                                  Mental = Score_MCS_num),
              Classe = data.frame(Physique = Score_PCS_cl,
                                  Mental = Score_MCS_cl)))
}


