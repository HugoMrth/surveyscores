\title{
  Post-traumatic Stress Disorder Checklist Scale
}\name{PCLS}
\alias{PCLS}
\description{
  Compile le score PCLS (Post-traumatic Stress Disorder Checklist Scale). Retourne les valeurs numériques et les classes associées selon les seuils fixes par la literature. .
}\usage{
  PCLS(x, inv.scale = FALSE)
}\arguments{
  \item{x}{
    La matrice des réponses aux 17 questions nécessaires à la construction du score. La matrice peut etre en numerique ou facteur, sans ordre precis dans les questions.
    Le calcul du score assume que la modalite l'intensite la plus basse correspond au facteur le plus bas (ou 1 en numerique). L'echelle va donc de 1 (pas du tout) a 5 (extremement).
}
\item{inv.scale}{
    Si \code{inv.scale = TRUE}, l'echelle est inversee pour le calcul.
    A utilise si l'echelle des donnees va du plus intense (1) au moins intense (5). Par defaut, \code{inv.scale = FALSE}.
}
}
\value{
  Une liste contenant un vecteur de scores et un vecteur de classes associes.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Hugo Marthinet
}\examples{
X <- matrix(data = sample.int(5, size = 50*17, replace = TRUE),
            ncol = 17)

PCLS(X)
table(PCLS(X)$Classe)
}
