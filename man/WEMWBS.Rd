\title{
  Warwick-Edinburgh Mental Well-being Scale
}\name{WEMWBS}
\alias{WEMWBS}
\description{
  Compile le score WEMWBS (Warwick-Edinburgh Mental Well-being Scale) de bien etre mental. Retourne les valeurs du score. Aucun seuil de reference n'existe pour ce score et il est plutot pertinent lors de la comparaison de plusieurs populations.
}\usage{
  WEMWBS(x, inv.scale = FALSE)
}\arguments{
  \item{x}{
    La matrice des réponses aux 14 questions nécessaires à la construction du score. La matrice peut etre en numerique ou facteur.
    Le calcul du score assume que la frequence la plus basse correspond au facteur le plus bas (ou 1 en numerique).
    L'echelle va donc de 1 (moins frequent) a 5 (plus frequent).
}  \item{inv.scale}{
    Si \code{inv.scale = TRUE}, l'echelle est inversee pour le calcul.
    A utilise si l'echelle des donnees va du plus frequent (1) au moins frequent (5). Par defaut, \code{inv.scale = FALSE}.
}
}
\value{
  Un vecteur de scores.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Hugo Marthinet
}\examples{
X <- matrix(data = sample.int(5, size = 50*14, replace = TRUE),
            ncol = 14)

WEMWBS(X)
quantile(WEMWBS(X), c(0.25, 0.75))
}
