\title{
  ProQOL - PROfessionnal Quality Of Life
}\name{ProQOL}
\alias{ProQOL}
\description{
  Compile les scores ProQOL (PROfessionnal Quality Of Life) de Satisfaction de la compassion, Épuisement professionnel et Stress traumatique secondaire. Retourne les valeurs numériques et les classes associées selon les seuils fixes par la literature.
}\usage{
  ProQOL(x, inv.scale = FALSE)
}\arguments{
  \item{x}{
    La matrice des réponses aux 30 questions nécessaires à la construction des scores. La matrice peut etre en numerique ou facteur, et doit respecter l'ordre des questions du questionnaire de reference.
    Le calcul du score assume que la modalite correspondant a la frequence la plus basse correspond au facteur le plus bas (ou 1 en numerique). L'echelle va donc de 1 (le moins frequent) a 5 (le plus frequent).
}
\item{inv.scale}{
    Si \code{inv.scale = TRUE}, l'echelle est inversee pour le calcul.
    A utilise si l'echelle des donnees va du plus frequent (1) au moins frequent (5). Par defaut, \code{inv.scale = FALSE}.
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
X <- matrix(data = sample.int(5, size = 50*30, replace = TRUE),
            ncol = 30)

ProQOL(X)
apply(ProQOL(X)$Classe, 2, table)
}
