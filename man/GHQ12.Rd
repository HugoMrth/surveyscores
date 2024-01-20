\title{
  General Health Questionary (12 questions)
}\name{GHQ12}
\alias{GHQ12}
\description{
  Compile le score GHQ (General Health Questionary) en 12 questons. Il permet d’évaluer le ressenti du répondant, ici au cours des 7 derniers jours, par rapport à son état habituel et ainsi la survenue d’une détresse psychologique à court terme. Retourne les valeurs numériques et les classes associées selon la méthode standard de binarisation entre les modalites positive et negatives..
}\usage{
  GHQ12(x, inv.scale = FALSE)
}\arguments{
  \item{x}{
    La matrice des réponses aux 12 questions nécessaires à la construction du score. La matrice peut etre en numerique ou facteur.
    Le calcul du score assume que la modalite la plus positive correspond au facteur le plus bas (ou 1 en numerique).
    L'echelle va donc de 1 (le plus positif) a 4 (le plus negatif).
}  \item{inv.scale}{
    Si \code{inv.scale = TRUE}, l'echelle est inversee pour le calcul.
    A utilise si l'echelle des donnees va du moins plus negatif (1) au plus positif (4). Par defaut, \code{inv.scale = FALSE}.
}
}
\value{
  Une liste contenant deux vecteurs : de scores et de classes associees.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Hugo Marthinet
}\examples{
X <- cbind(sample.int(4, size = 50, replace = TRUE),
           sample.int(4, size = 50, replace = TRUE),
           sample.int(4, size = 50, replace = TRUE),
           sample.int(4, size = 50, replace = TRUE),
           sample.int(4, size = 50, replace = TRUE),
           sample.int(4, size = 50, replace = TRUE),
           sample.int(4, size = 50, replace = TRUE),
           sample.int(4, size = 50, replace = TRUE),
           sample.int(4, size = 50, replace = TRUE),
           sample.int(4, size = 50, replace = TRUE),
           sample.int(4, size = 50, replace = TRUE),
           sample.int(4, size = 50, replace = TRUE))

GHQ12(X)
table(GHQ12(X)$Classe)
}
