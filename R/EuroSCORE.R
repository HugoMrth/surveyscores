EuroSCORE <- function(x = NULL,
                      top_chir_tho,
                      top_poids_op1,
                      top_poids_op2,
                      top_poids_op3,
                      top_inter_urg,
                      top_im,
                      top_FEVG,
                      top_eta_preop,
                      top_endo,
                      top_ir_dialysis,
                      top_chir_car,
                      top_mauv_mob,
                      top_MRC,
                      top_AEC,
                      sexe_f,
                      age_poids,
                      top_diab,
                      top_angor,
                      top_cla_nyha_2,
                      top_cla_nyha_3,
                      top_cla_nyha_4) {
  if (!is.null(x)) {
    if(dim(x)[2] != 21) stop("La matrice d'entrée doit contenir 21 colonnes pour les 21 variables utilisees pour la construction du score.")
    x <- as.data.frame(x)
  } else (
    x <- cbind(top_chir_tho,
               top_poids_op1,
               top_poids_op2,
               top_poids_op3,
               top_inter_urg,
               top_im,
               top_FEVG,
               top_eta_preop,
               top_endo,
               top_ir_dialysis,
               top_chir_car,
               top_mauv_mob,
               top_MRC,
               top_AEC,
               sexe_f,
               age_poids,
               top_diab,
               top_angor,
               top_cla_nyha_2,
               top_cla_nyha_3,
               top_cla_nyha_4)
  )

  
  levels(x[,1]) <- c(0.2196434, 0)
  levels(x[,2]) <- c(0.0285181, 0)
  levels(x[,3]) <- c(0.6527205, 0)
  levels(x[,4]) <- c(0.0062118, 0)
  levels(x[,5]) <- c(0.5521478, 0)
  levels(x[,6]) <- c(0.9724533, 0)
  levels(x[,7]) <- c(0.3174673, 0)
  levels(x[,8]) <- c(0.1528943, 0)
  levels(x[,9]) <- c(0.3150652, 0)
  levels(x[,10]) <- c(1.0865170, 0)
  levels(x[,11]) <- c(0.6194522, 0)
  levels(x[,12]) <- c(0.6421508, 0)
  levels(x[,13]) <- c(1.1185990, 0)
  levels(x[,14]) <- c(0.2407181, 0)
  levels(x[,15]) <- c(0.1886564, 0)
  levels(x[,16]) <- c(0.5360268, 0)
  levels(x[,17]) <- c(0.3542749, 0) 
  levels(x[,18]) <- c(0.2226147, 0)
  levels(x[,19]) <- c(0.1070545, 0)
  levels(x[,20]) <- c(0.2958358, 0)
  levels(x[,21]) <- c(0.5597929, 0)
  
  euroscore <- -5.324537 + rowSums(apply(x, 2, as.numeric))
  euroscore <- 100*exp(euroscore)/(1+exp(euroscore))
}
