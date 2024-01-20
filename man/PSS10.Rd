\title{
  Perceived Stress Scale (10 questions)
}\name{PSS10}
\alias{PSS10}
\description{
  Compile le score PSS10 (Perceived Stress Scale) de stress percu. Retourne les valeurs numériques et les classes associées selon les seuils fixes par la literature.
}\usage{
  PSS10(x, inv.scale = FALSE)
}\arguments{
  \item{x}{
    La matrice des réponses aux 10 questions nécessaires à la construction du score. La matrice peut etre en numerique ou facteur.
    Le calcul du score assume que la frequence la plus haute correspond au facteur le plus bas (ou 1 en numerique).
    L'echelle va donc de 1 (plus frequent) a 5 (moins frequent).
}  \item{inv.scale}{
    Si \code{inv.scale = TRUE}, l'echelle est inversee pour le calcul.
    A utilise si l'echelle des donnees va du moins frequent (1) au plus frequent (5). Par defaut, \code{inv.scale = FALSE}.
}
}
\value{
  Une liste contenant un matrice de scores (les deux sous score et le score global) et un vecteur de classes associes.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Hugo Marthinet
}\examples{
X <- matrix(data = sample.int(5, size = 50*10, replace = TRUE),
            ncol = 10)
PSS10(X)
table(PSS10(X)$Classe)
}
