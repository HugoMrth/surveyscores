EDSR <- function(x, inv.scale = FALSE) {
  x <- as.data.frame(x)
  if(dim(x)[2] != 21) stop("La matrice d'entrée doit contenir 21 colonnes pour les 21 questions utilisees pour la construction du score.")
  if(any(apply(x, 2, function(x) {length(table(x))}) > 6)) stop("Au moins une des questions possede plus de 6 niveaux")

  if (inv.scale) x <- apply(x, 2, function(y) {7 - as.numeric(y)})

  Score_EDSR_Sevrage <-
    rowSums(sapply(x[, c(1, 8, 15)], as.numeric), na.rm = TRUE)
  Score_EDSR_Continuite <-
    rowSums(sapply(x[, c(2, 9, 16)], as.numeric), na.rm = TRUE)
  Score_EDSR_Tolerance <-
    rowSums(sapply(x[, c(3, 10, 17)], as.numeric), na.rm = TRUE)
  Score_EDSR_Manque_de_Controle <-
    rowSums(sapply(x[, c(4, 11, 18)], as.numeric), na.rm = TRUE)
  Score_EDSR_Reduction_Autres_Activites <-
    rowSums(sapply(x[, c(5, 12, 19)], as.numeric), na.rm = TRUE)
  Score_EDSR_Temps <-
    rowSums(sapply(x[, c(6, 13, 20)], as.numeric), na.rm = TRUE)
  Score_EDSR_Intention <-
    rowSums(sapply(x[, c(7, 14, 21)], as.numeric), na.rm = TRUE)

  Score_EDSR_num <- rowSums(sapply(data.frame(cbind(Score_EDSR_Sevrage, Score_EDSR_Continuite,
                                         Score_EDSR_Tolerance, Score_EDSR_Manque_de_Controle,
                                         Score_EDSR_Reduction_Autres_Activites, Score_EDSR_Temps,
                                         Score_EDSR_Intention)), as.numeric), na.rm = TRUE)

  #### Score global EDS-R ####

  Dim_Sevrage <- rep(1, length(Score_EDSR_num))
  Dim_Sevrage[apply(sapply(x[, c(1, 8, 15)], as.numeric), 1,
                    function(x) {all(x>= 3)})] <- 2
  Dim_Sevrage[apply(sapply(x[, c(1, 8, 15)], as.numeric), 1,
                    function(x) {all(x>= 5)})] <- 3

  Dim_Continuite <- rep(1, length(Score_EDSR_num))
  Dim_Continuite[apply(sapply(x[, c(2, 9, 16)], as.numeric), 1,
                       function(x) {all(x>= 3)})] <- 2
  Dim_Continuite[apply(sapply(x[, c(2, 9, 16)], as.numeric), 1,
                       function(x) {all(x>= 5)})] <- 3

  Dim_Tolerance <- rep(1, length(Score_EDSR_num))
  Dim_Tolerance[apply(sapply(x[, c(3, 10, 17)], as.numeric), 1,
                      function(x) {all(x>= 3)})] <- 2
  Dim_Tolerance[apply(sapply(x[, c(3, 10, 17)], as.numeric), 1,
                      function(x) {all(x>= 5)})] <- 3

  Dim_Manque_de_Controle <- rep(1, length(Score_EDSR_num))
  Dim_Manque_de_Controle[apply(sapply(x[, c(4, 11, 18)], as.numeric), 1,
                               function(x) {all(x>= 3)})] <- 2
  Dim_Manque_de_Controle[apply(sapply(x[, c(4, 11, 18)], as.numeric), 1,
                               function(x) {all(x>= 5)})] <- 3

  Dim_Reduction_Autres_Activites <- rep(1, length(Score_EDSR_num))
  Dim_Reduction_Autres_Activites[apply(sapply(x[, c(5, 12, 19)], as.numeric), 1,
                                       function(x) {all(x>= 3)})] <- 2
  Dim_Reduction_Autres_Activites[apply(sapply(x[, c(5, 12, 19)], as.numeric), 1,
                                       function(x) {all(x>= 5)})] <- 3

  Dim_Temps <- rep(1, length(Score_EDSR_num))
  Dim_Temps[apply(sapply(x[, c(6, 13, 20)], as.numeric), 1,
                  function(x) {all(x>= 3)})] <- 2
  Dim_Temps[apply(sapply(x[, c(6, 13, 20)], as.numeric), 1,
                  function(x) {all(x>= 5)})] <- 3

  Dim_Intention <- rep(1, length(Score_EDSR_num))
  Dim_Intention[apply(sapply(x[, c(7, 14, 21)], as.numeric), 1,
                      function(x) {all(x>= 3)})] <- 2
  Dim_Intention[apply(sapply(x[, c(7, 14, 21)], as.numeric), 1,
                      function(x) {all(x>= 5)})] <- 3

  DIMS <- cbind(Dim_Sevrage, Dim_Continuite, Dim_Tolerance, Dim_Manque_de_Controle,
                Dim_Reduction_Autres_Activites, Dim_Temps, Dim_Intention)

  Score_EDSR_Global <- rep("Asymptomatique", length(Score_EDSR_num))
  Score_EDSR_Global[apply(DIMS, 1, function(x) {sum(x > 1) > 2})] <- "Symptomatique"
  Score_EDSR_Global[apply(DIMS, 1, function(x) {sum(x > 2) > 2})] <- "Dependant"




  return(list(Valeur = data.frame(Sevrage = Score_EDSR_Sevrage,
                                  Continuite = Score_EDSR_Continuite,
                                  Tolerance = Score_EDSR_Tolerance,
                                  `Manque de Controle` = Score_EDSR_Manque_de_Controle,
                                  `Reduction des autres activites`= Score_EDSR_Reduction_Autres_Activites,
                                  Temps = Score_EDSR_Temps,
                                  Intention = Score_EDSR_Intention),
              Dimension = data.frame(Sevrage = Dim_Sevrage,
                                  Continuite = Dim_Continuite,
                                  Tolerance = Dim_Tolerance,
                                  `Manque de Controle` = Dim_Manque_de_Controle,
                                  `Reduction des autres activites`= Dim_Reduction_Autres_Activites,
                                  Temps = Dim_Temps,
                                  Intention = Dim_Intention),
              Classe = Score_EDSR_Global))
}
