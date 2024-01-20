\title{
  Calcul d'IMC
}
\name{IMC}
\alias{IMC}
\description{
  Calcul la valeur d'IMC selon le poids et la taille et retourne la classe correspondante.
}\usage{
  IMC(taille, poids, round = TRUE, digits = 0)
}\arguments{
  \item{taille}{
    Un numérique ou vecteur de numérique correspondant à la taille en metre ou centimetre.
}
  \item{poids}{
    Un numérique ou vecteur de numérique correspondant au poids en kilogrammes.
}
  \item{round}{
    Choix d'arrondir l'IMC, par défaut l'arrondi est fait à un chiffre apres la virgule.
}
  \item{digits}{
    Nombre de chiffres apres la virgule de l'arrondi
}
}\value{
  Une liste constituee d'un vecteur numerique de valeur d'IMC, et d'un vecteur de châines de caracteres des classes correpondantes.
}\author{
  Joffrey Marchi
}\examples{
IMC(1.71, 69)
IMC(171, 69)

taille <- c(1.78, 1.56, 1.89, 1.67)
poids <- c(74, 56, 85, 96)

IMC(taille, poids)
IMC(taille*100, poids)
}
