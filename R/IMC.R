IMC <- function(taille, poids, round = TRUE, digits = 1){
  arrondi_loc <- function(x, digits = 0) {
    .local <- function(x, digits = 0) {
      x <- x * (10^digits)
      ifelse(abs(x%%1 - 0.5) < .Machine$double.eps^0.5, ceiling(x)/(10^digits),
             round(x)/(10^digits))
    }
    if (is.data.frame(x)) {
      return(data.frame(lapply(x, .local, digits)))
    }
    .local(x, digits)
  }

  # Si taille en centimètre
  if (all(taille > 100, na.rm = TRUE)) {
    taille <- taille / 100
  }

  if(round == TRUE){
    imc <- arrondi_loc(poids / (taille * taille), digits = digits)
  }else{
    imc <- (poids / (taille * taille))
  }

  imc_classe <- as.character(cut(imc,
                    breaks = c(-Inf, 16.5, 18.5, 25, 30, 35, 40, +Inf),
                    right = FALSE,
                    labels = c("Dénutrition", "Maigreur", "Poids normal", "Surpoids", "Obésité modérée", "Obésité sévère", "Obésité morbide")))
  return(list(Valeur = imc,
              Classe = imc_classe))
}

# IMC(1.71, 69)
# IMC(171, 69)
#
# taille <- c(1.78, 1.56, 1.89, 1.67)
# poids <- c(74, 56, 85, 96)
#
# IMC(taille, poids)
# IMC(taille*100, poids)
