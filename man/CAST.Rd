\title{
  Cannabis Abuse Screening Test
}\name{CAST}
\alias{CAST}
\description{
  Compile le score CAST (Cannabis Abuse Screening Test). Retourne les valeurs numériques et les classes associées selon les seuils fixes par la literature. Les questions utilisees pour la construction du score concernent : le fait de fumer avant midi, de fumer seul, d'avoir eu des problemes de memoire, que des proche aient conseille une diminution de la consommation, d'avoir essaye de reduire ou arreter la consommation, et d'avoir deja eu des problemes lies a la consommation.
}\usage{
  CAST(x, inv.scale = FALSE)
}\arguments{
  \item{x}{
    La matrice des réponses aux 6 questions nécessaires à la construction du score. La matrice peut etre en numerique ou facteur, sans ordre precis dans les questions.
    Le calcul du score assume que la modalite la frequence la plus basse correspond au facteur le plus bas (ou 1 en numerique). L'echelle va donc de 1 (le moins frequent) a 5 (le plus frequent).
}
\item{inv.scale}{
    Si \code{inv.scale = TRUE}, l'echelle est inversee pour le calcul.
    A utilise si l'echelle des donnees va du plus frequent (1) au moins frequent (5). Par defaut, \code{inv.scale = FALSE}.
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
X <- matrix(data = sample.int(5, size = 50*6, replace = TRUE),
            ncol = 6)

CAST(X)
table(CAST(X)$Classe)
}
