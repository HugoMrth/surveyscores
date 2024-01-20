\title{
  Hospital Anxiety and Depression Scale
}\name{HAD}
\alias{HAD}
\description{
  Compile le score HAD (Hospital Anxiety and Depression Scale) ainsi que ses deux sous scores d'anxiete et de depression. Retourne les valeurs numériques et les classes associées selon les seuils fixes par la literature.
}\usage{
  HAD(x,
      inv.anxiete = FALSE,
      inv.depression = FALSE)
}\arguments{
  \item{x}{
    La matrice des réponses aux 14 questions nécessaires à la construction du score HAD.
    La matrice peut etre en numerique ou facteur, mais les questions et leur facteurs doivent etre dans le bon ordre (cf. literature).
}  \item{inv.anxiete}{
    Pour le sous score d'anxiete, soit les questions impaires, le calcul assume que la fréquence la plus basse correspond au facteur le plus bas (ou 1 en numerique). L'echelle va donc de 1 (le moins frequent) a 4 (le plus frequent).
    Si \code{inv.anxiete = TRUE}, l'echelle du sous score d'anxiete est inversee pour le calcul. Par defaut, \code{inv.anxiete = FALSE}.
}  \item{inv.depression}{
    Pour le sous score de depression, soit les questions paires, le calcul assume que le niveau le plus positif correspond au facteur le plus bas (ou 1 en numerique). L'echelle va donc de 1 (le plus positif) a 4 (le plus negatif).
    Si \code{inv.depression = TRUE}, l'echelle du sous score de depression est inversee pour le calcul. Par defaut, \code{inv.anxiete = FALSE}.
}
}\value{
  Une liste de deux data.frames, le premier avec les scores numeriques, le deuxieme avec les classes associees.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Hugo Marthinet
}\examples{
X <- matrix(data = sample.int(4, size = 50*14, replace = TRUE),
            ncol = 14)


HAD(X)
lapply(HAD(X)$Class, table)
}
