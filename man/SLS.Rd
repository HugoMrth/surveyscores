\title{
  Score SLS (Short Loneliness Scale) de solitude
}\name{SLS}
\alias{SLS}
\description{
  Compile le score SLS (Short Loneliness Scale) de solitude en 3 questions. Retourne les valeurs numériques et les classes associées selon les seuils fixes par la literature. Les questions utilisees pour la construction du score concernent les fréquences de : manque de compagnie, sentiment de mise à l'écart et d'isolement, toutes en 4 classes.
}\usage{
  SLS(x, inv.scale = FALSE)
}\arguments{
  \item{x}{
    La matrice des réponses aux 3 questions nécessaires à la construction des scores. La matrice peut etre en numerique ou facteur.
    Le calcul du score assume que la modalite traduisant le moins d'isolement correspond au facteur le plus bas (ou 1 en numerique). L'echelle va donc de 1 (le moins d'isolement) a 4 (le plus d'isolement).
}
\item{inv.scale}{
    Si \code{inv.scale = TRUE}, l'echelle est inversee pour le calcul.
    A utilise si l'echelle des donnees va du plus d'isolement (1) au moins d'isolement (4). Par defaut, \code{inv.scale = FALSE}.
}
}
\value{
  Une liste de deux vecteurs, le premier avec les valeurs du score, le deuxieme avec les classes associees.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Hugo Marthinet
}\examples{
X <- cbind(sample.int(4, size = 50, replace = TRUE),
           sample.int(4, size = 50, replace = TRUE),
           sample.int(4, size = 50, replace = TRUE))

SLS(X)
}
