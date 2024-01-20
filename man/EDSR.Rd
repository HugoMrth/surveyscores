\title{
  EDS-R - Exercise Dependence Scale-Revised
}\name{EDSR}
\alias{EDSR}
\description{
  Compile le score EDS-R (Exercise Dependence Scale-Revised) d'addiction au sport. Prend en sompte les sept dimensions comprenant chacune 3 items (échelle de Likert de 1 à 6). Le score global est construit à partir des 7 dimensions et s’interprète de la manière suivante :
  Score >= 5 à toutes les questions d’au moins trois dimensions : Dépendant,
  Score >= 3 à toutes les questions d’au moins trois dimensions et non dépendant : Symptomatique,
  Ni dépendant, ni symptomatique : Asymptomatique
}\usage{
  EDSR(x, inv.scale = FALSE)
}\arguments{
  \item{x}{
    La matrice des réponses aux 21 questions nécessaires à la construction des scores. La matrice peut etre en numerique ou facteur, et doit respecter l'ordre des questions du questionnaire de reference.
    Le calcul du score assume que la modalite correspondant a la frequence la plus basse correspond au facteur le plus bas (ou 1 en numerique). L'echelle va donc de 1 (le moins frequent) a 6 (le plus frequent).
}
\item{inv.scale}{
    Si \code{inv.scale = TRUE}, l'echelle est inversee pour le calcul.
    A utilise si l'echelle des donnees va du plus frequent (1) au moins frequent (6). Par defaut, \code{inv.scale = FALSE}.
}
}\value{
  Une liste de deux data.frames (le premier avec les scores numeriques pour chacune des 7 dimensions, le deuxieme avec les classes associees) et d'un vecteur des classes du score global.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Hugo Marthinet
}\examples{
X <- matrix(data = sample.int(6, size = 50*21, replace = TRUE),
            ncol = 21)

EDSR(X)
table(EDSR(X)$Classe)
}
