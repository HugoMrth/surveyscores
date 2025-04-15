MRMI <- function(x = NULL,
                 genre
                     age,
                     isch_heart,
                     cerebro,
                     heart_fail,
                     periph_vasc,
                     diabetes,
                     cancer,
                     cancer_hist,
                     schiz,
                     depression
                     substance_abuse,
                     dementia,
                     parkinson,
                     sclerosis,
                     epilepsy,
                     chronic_resp,
                     infl_bowel,
                     arthritis,
                     AIDS,
                     end_renal,
                     liver_pancreas
) {
  if (!is.null(x)) {
    x <- as.data.frame(x)
    if(dim(x)[2] != 22) stop("La matrice doit les 22 comorbidites DANS L'ORDRE.")
    if(any(apply(x[, -1], 2, function(x) {length(table(x))}) > 2)) stop("Au moins une des comorbidites possede plus de 2 niveauxs.")
  } else {
    x <- cbind(genre
               age,
               isch_heart,
               cerebro,
               heart_fail,
               periph_vasc,
               diabetes,
               cancer,
               cancer_hist,
               schiz,
               depression
               substance_abuse,
               dementia,
               parkinson,
               sclerosis,
               epilepsy,
               chirnic_resp,
               infl_bowel,
               arthritis,
               AIDS,
               end_renal,
               liver_pancreas)
    if(any(apply(x[, -1], 2, function(x) {length(table(x))}) > 2)) stop("Au moins une des comorbidites possede plus de 2 niveauxs.")
  }
  
  
  x[, 2] <- floor((x[, 2]-65)/5)
  x[, 2] <- ifelse(x[, 2] < 0, 0, ifelse(x[, 2] > 7, 7, x[, 2]))
  MRMI <- x[, 2] +
    0 * rowSums(x[, c(3, 9, 18, 20)]) +
    1 * rowSums(x[, c(1, 4, 5, 6, 7, 10, 11, 14, 16, 17, 19)]) + 
    2 * rowSums(x[, c(12, 13, 15, 21, 22)]) + 
    3 * x[, 8]
  
  ERMI <- x[, 2] +
    0 * rowSums(x[, c(1, 18)]) +
    2 * rowSums(x[, c(3, 9, 13)]) + 
    3 * rowSums(x[, c(4, 5, 6, 16 ,17)]) + 
    4 * rowSums(x[, c(7, 19)]) + 
    5 * rowSums(x[, c(11, 12, 14, 22)]) + 
    6 * x[, 10] +
    7 * x[, 8] +
    9 * x[, 15] +
    10 * x[, 20] +
    16 * x[, 21]

  return(list(MRMI = MRMI,
              ERMI = ERMI))
}
